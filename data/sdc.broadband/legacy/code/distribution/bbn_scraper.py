# imports
# generic imports
import numpy as np
import re
import os
import time
import pandas as pd
import random
from bs4 import BeautifulSoup
from tqdm import tqdm
from slugify import slugify
import math
import argparse
from pprint import pprint
import traceback
import logging

# selenium imports
import selenium
from selenium import webdriver
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import ElementClickInterceptedException
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import TimeoutException


# check if xpath exists, if not return false
def check_exists_by_xpath(driver, xpath):
    """
    Description:
        Check existence of xpath on page

    Inputs:
        webdriver: your webdriver
        xpath: whatever element we are looking for

    Outputs:
        returns True if xpath exists, False if not
    """
    # try to find element
    try:
        # driver.find_element_by_xpath(xpath)
        driver.find_element("xpath", xpath)

    # throw exception and return false if unable to find
    except NoSuchElementException:
        return False
    return True


# IMPORTANT: Corelogic provides BLOCK level FIPs code, but we use BLOCK GROUP level data here
def read_and_clean_addresses_for_bgs(data, need_subset=True, size_subset=3):
    """
    Description:
        Check existence of xpath on page

    Inputs:
        data: string, name of csv you want to use (includes .csv ending)
        need_subset: boolean, True if using subset of data (originally used 1st address within each bg from list of 3) - default = True
        size_subset: integer, if subsetting, selects every "nth" row (not necessary to mess with this param if using 1 address per bg) - default = 3

    Outputs:
        returns True if xpath exists, False if not
    """
    # read in csv, drop index, and update block column
    address_sample_3_per_bg = pd.read_csv(data, index_col=0)
    address_sample_3_per_bg = address_sample_3_per_bg.reset_index(drop=True)
    address_sample_3_per_bg["geoid_blk"] = address_sample_3_per_bg.geoid_blk.astype(str)

    # drop lat 4 digits of mail address to get short zipcode
    a = address_sample_3_per_bg.mail_address.values
    a = np.array([a[i][0:-4] if a[i][-9].isdigit() else a[i] for i in range(len(a))])

    # get block group geoid
    address_sample_3_per_bg["geoid_bg"] = address_sample_3_per_bg.geoid_blk.str.slice(
        start=0, stop=12
    )

    # if data needs subsetting (I had 3 addresses )
    if need_subset:
        addresses = a[::size_subset]
        block_geoids = address_sample_3_per_bg.geoid_bg[::size_subset]

    else:
        addresses = a
        block_geoid = address_sample_3_per_bg.geoid_bg

    return addresses, block_geoids.values


def search_address2(address, driver, driver_wait=20):
    """
    Description:
        Check existence of xpath on page

    Inputs:
        address: string, single home address we are scraping for
        driver: your webdriver
        driver_wait: integer, wait time for driver - default = 20

    Outputs:
        returns True if xpath exists, False if not
    """
    # wait until search bar is clickable and enter address
    wait = WebDriverWait(driver, driver_wait)
    search = wait.until(EC.element_to_be_clickable((By.ID, "plan-search")))
    search.clear()
    search.send_keys("{}".format(address))

    # sleep, then go to top suggested address
    time.sleep(2)
    go_top = check_exists_by_xpath(
        driver, '//*[@id="plans-search"]/div/div/div[1]/div/div/div/ul'
    )

    # click top address
    if go_top:
        go_top_address = wait.until(
            EC.element_to_be_clickable(
                (By.XPATH, '//*[@id="plans-search"]/div/div/div[1]/div/div/div/ul/li')
            )
        )
        go_top_address.click()

    return go_top


def extract_page(address, driver):
    # bs - scrape page
    html = driver.page_source
    soup = BeautifulSoup(html)

    errors = soup.find_all(attrs={"class": "c-plans-search-error__heading"})

    plan_dfs = []
    # Iterate through each service plan:
    for plan in soup.find_all(attrs={"class": "l-providers-list__item"}):
        # Create empty data frame
        plan_df = pd.DataFrame()
        speed = [
            s.getText()
            for s in plan.find_all(attrs={"class": "c-provider-card__speeds-value"})
        ]
        down_up = [
            s.getText()
            for s in plan.find_all(attrs={"class": "c-provider-card__speeds-label"})
        ]
        price = (
            plan.find(attrs={"class": "c-provider-card__plan-value"})
            .getText()
            .split("$")[-1]
        )
        name = (
            plan.find(attrs={"class": "c-provider-card__provider-name"})
            .getText()
            .split(". ")[1]
        )
        internet_type = (
            plan.find(attrs={"class": "c-provider-card__label"}).getText().strip()
        )

        plan_df["speed"] = speed
        plan_df["down_up"] = down_up
        plan_df["price"] = price
        plan_df["name"] = name
        plan_df["type"] = internet_type
        plan_df["address"] = address
        plan_df["success"] = True

        if plan_df is not None and not plan_df.empty:
            plan_dfs.append(plan_df)

    df = pd.concat(plan_dfs)
    # print(df)
    return df


def scrape_prices(
    driver, wait, addresses, min_wait=10, max_wait=30, save_folder="../../data/temp/"
):
    """
    Description:
        Scrape internet packages from Broadbandnow.com - takes each address and scrapes all packages for top match

    Inputs:
        driver: your webdriver
        addresses: array of strings, home addresses we are scraping for (first output of read_and_clean_addresses_for_bgs)

    Outputs:
        df: a data frame containing the columns:
            address: corresponding address of the package
            price: price of the package
            name: name of the package
            type: type of package?
            speed: speed of the package

    """
    adfs = []
    empty = []

    # loop over block group addressed
    for address in tqdm(addresses):
        # try below and exception IF takes too long (increments a counter before skipping address eventually)

        address_name = slugify(address)
        if os.path.isfile(save_folder + address_name + ".csv"):
            continue

        try:
            # reload page to clear results (noticed that we run into issues if we do not clear)
            driver.get("https://broadbandnow.com/compare/plans")
            go_top = search_address2(address, driver)

            # select top address
            if not go_top:
                # print('Skipping: Cannot go to the top address')
                empty.append(address)
                continue  # skip to next address
            time.sleep(1)
            unable_to_confirm = check_exists_by_xpath(
                driver,
                "/html/body/div[2]/div/div/div[1]/section/section/div/div/div[1]/div/section",
            )

            # if able to confirm and go to top axrddress
            if unable_to_confirm:
                # print('Skipping: Cannot go to the top address')
                empty.append(address)
                continue

            time.sleep(1)
            load_more = check_exists_by_xpath(
                driver, '//*[@id="cityPlansListing"]/section/div/div[2]/div/div/section'
            )

            # if load more is an option, then load all packages
            if load_more:
                # load all plans
                load_all_plans = wait.until(
                    EC.element_to_be_clickable(
                        (
                            By.XPATH,
                            '//*[@id="cityPlansListing"]/section/div/div[2]/div/div/section',
                        )
                    )
                )
                load_all_plans.click()

            adf = extract_page(address, driver)

            if not adf.empty:
                # adfs.append(adf)
                adf.to_csv(save_folder + address_name + ".csv", index=False)

            # Be respectful of pinging the server
            # time.sleep(random.randint(min_wait, max_wait))

            # select edit option to change address
            edit = wait.until(
                EC.element_to_be_clickable(
                    (By.XPATH, '//*[@id="plans-search"]/div/div/div/h1/span')
                )
            )
            edit.click()

        # if try fails, throw exception and increment counter (retry until problem_counter hits 5)
        # throws error if we try to edit search plans but this is not an option because nothing was searched after hitting home page
        except TimeoutException as ex:
            # DO something
            empty.append(address)

    # close driver
    driver.quit()

    return empty


def main(input_file, output_dir, headless=False):
    # start driver
    options = Options()
    # Uncomment to run headless (i.e., don't show the browser)
    if headless:
        options.add_argument("--headless")
    # options.add_argument('--disable-gpu')  # Last I checked this was necessary.
    driver = webdriver.Chrome(ChromeDriverManager().install(), chrome_options=options)
    driver.get("https://broadbandnow.com/compare/plans")
    # driver.maximize_window()

    # set driver params
    driver_wait = 5
    sleep_time = 2
    wait = WebDriverWait(driver, driver_wait)

    df = pd.read_csv(input_file)

    # cleaning addresses
    addresses = list(df["address"].unique())
    addresses = [v for v in addresses if isinstance(v, str)]

    # save the valid file paths
    addresses = [
        a for a in addresses if not slugify(a) + ".csv" in os.listdir(output_dir)
    ]
    empty = scrape_prices(driver, wait, addresses, 3, 5, save_folder=output_dir)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Given a csv with an address column in it, iterate through all of the addresses and return result of its query from broadbandnow"
    )
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="The input csv",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_dir",
        type=str,
        help="The output directory",
        required=True,
    )
    parser.add_argument("-v", "--verbose", action=argparse.BooleanOptionalAction)
    parser.add_argument(
        "-h",
        "--headless",
        default=False,
        help="whether or not to run the browser in headless mode or not",
        action=argparse.BooleanOptionalAction,
    )

    args = parser.parse_args()
    log_level = logging.INFO
    if args.verbose:
        log_level = logging.DEBUG

    logging.basicConfig(format="%(levelname)s: %(message)s", level=log_level)

    if not os.path.isfile(args.input_file) or not os.path.isdir(args.output_dir):
        logging.info(
            "[%s] Input file valid: %s"
            % (os.path.isfile(args.input_file), args.input_file)
        )
        logging.info(
            "[%s] Ouput dir valid: %s"
            % (os.path.isdir(args.output_dir), args.output_file)
        )

    import warnings

    warnings.filterwarnings("ignore")
    main(args.input_file, args.output_dir, args.headless)
