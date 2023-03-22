### IMPORTS
# generic imports
import numpy as np
import re
import time
import pandas as pd
import matplotlib.pyplot as plt

# importing selenium related packages
import selenium
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import ElementClickInterceptedException
from selenium.common.exceptions import NoSuchElementException

# Exception Checks
def check_exists_by_xpath(webdriver, xpath):
    try:
        webdriver.find_element_by_xpath(xpath)
    except NoSuchElementException:
        return False
    return True

def check_exists_by_class_name(webdriver, class_name):
    try:
        webdriver.find_element_by_class_name(class_name)
    except NoSuchElementException:
        return False
    return True

def check_intercepted_by_xpath(webdriver, xpath):
    try:
        webdriver.find_element_by_xpath(xpath)
    except ElementClickInterceptedException:
        return True
    return False

def check_intercepted_by_class_name(webdriver, class_name):
    try:
        webdriver.find_element_by_class_name(class_name)
    except ElementClickInterceptedException:
        return True
    return False

### GIANT
def giant_locations(zipcodes, bad_zip = [22216, 22241, 22245], sleep_time = 2, driver_wait = 20):
    '''
    Description:
        Get shortened list of zipcodes and locations for Giant: shortened_zipcodes AND shortened_locations
    
    Inputs:
        zipcodes: list of Arlington zipcodes
        bad_zip: list of zipcodes for which there were invalid searches (not sure if these are still valid zipcodes)
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        
    Outputs:
        shortened_zipcodes: array of zipcodes associated with unique stores
        shortened_locations: array of unique store locations close to Arlington
    '''
    # Create Giant driver and go to website
    driver = webdriver.Chrome(ChromeDriverManager().install())
    driver.get("https://giantfood.com/")
#     driver.maximize_window()
    
    # close initial popup w X button
    time.sleep(sleep_time)
    if check_exists_by_class_name(driver, "modal_close"):
        x_button = driver.find_element_by_class_name("modal_close")
        x_button.click()

    # close second popup w X button
    time.sleep(sleep_time)
    if check_exists_by_xpath(driver, '//button[@aria-label="close dialog"]'):
        x_button = driver.find_element_by_xpath('//button[@aria-label="close dialog"]')
        x_button.click()

    # Change shopping mode to find stores
    change = driver.find_element_by_xpath('//button[@aria-label="Change shopping mode"]')
    change.click()

    # choose to enter a zip code
    wait = WebDriverWait(driver, driver_wait)
    enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '//button[@class="button button--prime pdl-service-selector_btn"]')))
    enter_zip.click()

    # distances and locations lists
    distances = []; locations = []

    # zipcodes without stores for whatever reason but still listed as Arlington
    for zipcode in zipcodes:
        if(zipcode not in bad_zip):
            # enters new zipcode
            new_zip = wait.until(EC.element_to_be_clickable((By.ID, 'search-zip-code')))
            new_zip.clear()
            new_zip.send_keys(str(zipcode))

            # finds stores by zipcode
            find_stores = wait.until(EC.element_to_be_clickable((By.ID, 'search-location')))
            find_stores.click()
            time.sleep(sleep_time)
            find_stores.click()
            time.sleep(sleep_time)

            # append distances and locations to lists
            distances.append(driver.find_element_by_xpath('//li//span[1]').text)
            locations.append(driver.find_element_by_xpath('//li//span[2]').text + ", " + driver.find_element_by_xpath('//li//span[3]').text)

    # close driver
    driver.quit()

    # getting zipcodes with different closest Giant
    _, idxs = np.unique(locations, return_index = True)
    
    # dropping zipcodes with no closest GIANT -- Might change if some zipcodes are dropped
    shortened_zipcodes = zipcodes[idxs][1:]
    shortened_locations = _[1:]
    return shortened_zipcodes, shortened_locations

def giant_driver(shortened_zipcodes, giant_foods, sleep_time = 2, driver_wait = 20, standard = True):
    '''
    Description: 
        Compiles list of products across the unique Giants based on the food items based in as input
    
    Inputs:
        shortened_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Giant
        giant_foods: array of food items
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        standard: Boolean, True if using "Universal" basket of goods
    
    Outputs:
        items_by_zip: 2D array of food items by store
    '''
    # loop over shortened list of zipcodes, collecting staples
    items_by_zip = []
    for i, zipcode in enumerate(shortened_zipcodes):
        # GIANT FOOD List: items_by_zip AND shortened_locations
        # create driver and go to driver website
        driver = webdriver.Chrome(ChromeDriverManager().install())
        driver.get("https://giantfood.com/")
#         driver.maximize_window()
        
        # close popups
        if check_exists_by_class_name(driver, "modal_close"):
            x_button = driver.find_element_by_class_name("modal_close")
            x_button.click()
            
        if check_exists_by_xpath(driver, '//button[@aria-label="close dialog"]'):
            x_button_2 = driver.find_element_by_xpath('//button[@aria-label="close dialog"]')
            x_button_2.click()

        # change shopping mode
        wait = WebDriverWait(driver, driver_wait)
        change = wait.until(EC.element_to_be_clickable((By.XPATH, '//button[@aria-label="Change shopping mode"]')))
        change.click()

        # select option to enter zipcode
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[3]/aside/div/div/div/section/div/div/div[1]/div[1]/div[1]/div/div[3]/div/button')))
        enter_zip.click()

        # pass in new zipcode
        new_zip = wait.until(EC.element_to_be_clickable((By.ID, 'search-zip-code')))
        new_zip.clear()
        new_zip.send_keys(str(zipcode))

        # select store location (has to click 2x to go through)
        find_stores = driver.find_element_by_id('search-location')
        find_stores.click()
        time.sleep(sleep_time)
        find_stores.click()
        time.sleep(sleep_time)

        # go to the top store selected
        go_top_store = driver.find_element_by_xpath('/html/body/div[1]/div[3]/aside/div/div/div/section/div/div/div/div[2]/div[3]/ul/li[1]/div/div[3]/button')
        go_top_store.click()

        # loop over staplees
        items = []
        for food in giant_foods:
            # search for food
            time.sleep(sleep_time)
            search_bar = driver.find_element_by_id("typeahead-search-input")
            search_bar.clear()
            search_bar.send_keys(food)

            # select that food item
            enter_food = driver.find_element_by_xpath('//button[@class="button search-button button--prime"]')
            enter_food.click()
            
            # if standard basket
            if standard:
                # for select foods in the standard basket - sort by price
                if (food == giant_foods[1]) or (food == giant_foods[2]) or (food == giant_foods[6]) or (food == giant_foods[7]):
                    time.sleep(sleep_time)
                    best_match = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[4]/div/div/div/div/div[2]/div[1]/div/div[2]/div/div/div/div[1]/div/div[2]/div[2]/div')))
                    best_match.click()
                    
                    option = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[4]/div/div/div/div/div[2]/div[1]/div/div[2]/div/div/div/div[1]/div/div[2]/div[2]/div/div/select/option[3]')))
                    option.click()

            #
            if (food == 'Garbanzo Beans'):
                time.sleep(sleep_time)
                best_match = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[4]/div/div/div/div/div[2]/div[1]/div/div[2]/div/div/div/div[1]/div/div[2]/div[2]/div')))
                best_match.click()
                option = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[4]/div/div/div/div/div[2]/div[1]/div/div[2]/div/div/div/div[1]/div/div[2]/div[2]/div/div/select/option[2]')))
                option.click()
    
            #
            if (food == "Black Eye Peas") or (food == "Pinto Beans"):
                time.sleep(sleep_time)
                best_match = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[4]/div/div/div/div/div[2]/div[1]/div/div[2]/div/div/div/div[1]/div/div[2]/div[2]/div')))
                best_match.click()
                option = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[4]/div/div/div/div/div[2]/div[1]/div/div[2]/div/div/div/div[1]/div/div[2]/div[2]/div/div/select/option[3]')))
                option.click()

            # obtain price and name
            time.sleep(sleep_time)
            element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'product-tile_content')))
            items.append(element.text)

        # remove add to cart and append to list
        items = np.array([item.replace('\nAdd to Cart', '') for item in items])
        items_by_zip.append(items)
        
        # close driver
        driver.quit()
    
    # convert from list to array
    items_by_zip = np.array(items_by_zip)
    return items_by_zip

def giant_price_item_other(items_by_zip):
    '''
    Description:
        Extracts food item, price and other information from 2D array of information passed as input
    
    Input:
        items_by_zip: 2D array of food items by store (contains other information)
    
    Output:
        prices_giant: 2D array of prices for each item at each store
        cleaned_items_giant: 2D array of products at each store
        other_info_giant: 2D array of additional product information
    '''
    # clean strings and separate price, items, and other information
    if_sale_giant = np.array([np.array([1 if items_by_zip[j][i][:4] == "SALE" else 0 for i in range(len(items_by_zip[j]))]) for j in range(len(items_by_zip))])
    no_sale_giant = np.array([np.array([items_by_zip[j][i].split("SALE\n")[-1] for i in range(len(items_by_zip[j]))])  for j in range(len(items_by_zip))])
    prices_giant = np.array([np.array([float(no_sale_giant[j][i].split("\n")[0].replace("$", "")) for i in range(len(no_sale_giant[j]))]) for j in range(len(no_sale_giant))])

    cleaned_giant = np.array([np.array(["\n".join(no_sale_giant[j][i].split("\n")[:1] + no_sale_giant[j][i].split("\n")[2:]) if if_sale_giant[j][i] == 1
                        else no_sale_giant[j][i] for i in range(len(no_sale_giant[j]))]) for j in range(len(no_sale_giant))])
    cleaned_items_giant = np.array([np.array([cleaned_giant[j][i].split("\n")[1] for i in range(len(cleaned_giant[j]))]) for j in range(len(cleaned_giant))])
    other_info_giant = np.array([np.array([cleaned_giant[j][i].split("\n")[2] if len(cleaned_giant[j][i].split("\n")) > 2 else "" for i in range(len(cleaned_giant[j]))]) for j in range(len(cleaned_giant))])

    return prices_giant, cleaned_items_giant, other_info_giant
    
def make_df(prices, items, other, locations, store):
    '''
    Description:
        Produces dataframe from inputs with info on price, item, location, etc.
    
    Input:
        prices: 2D array of prices for each item at each store
        items: 2D array of products at each store
        other: 2D array of additional product information
        locations: 1D array of store locations
        store: Name of supermarket chain
    
    Output:
        df: dataframe with inputs as 5 columns
    '''
    # create dataframe columns - flattening several 2D arrays
    a = np.array([item for sublist in items for item in sublist])
    b = np.array([item for sublist in prices for item in sublist])
    c = np.array([item for sublist in other for item in sublist])
    d = np.array([locations[i] for i in range(len(locations)) for j in range(len(items[0]))])
    e = np.array([store for i in range(len(locations)) for j in range(len(items[0]))])

    # put dataframe columns together and return dataframe
    df = pd.DataFrame({"Item": a, "Price": b, "Other_Info": c, "Location": d, "Store": e})
    return df

# from scratch function from foods to dataframe: giant
def giant(foods, zipcodes, standard = True, has_zipcodes = False, locs = []):
    '''
    Description:
        Runs combination of functions to go from a list of zipcodes and food items to final dataframe for Giant
    
    Inputs:
        foods: array of food items
        zipcodes: list of Arlington zipcodes
        standard: Boolean, True if using "Universal" basket of goods
        
    Outputs:
        df: dataframe with price, info, etc.
    '''
    # if shortened zipcodes list passed in
    if has_zipcodes:
        shortened_zipcodes = zipcodes
        shortened_locations = locs
    
    # get shortened list of zipcodes if not provided
    else:
        shortened_zipcodes, shortened_locations = giant_locations(zipcodes)
    
    items_by_zip = giant_driver(shortened_zipcodes, foods, standard = standard)
    prices_giant, cleaned_items_giant, other_info_giant = giant_price_item_other(items_by_zip)
    df = make_df(prices_giant, cleaned_items_giant, other_info_giant, shortened_locations, "Giant")
    
    # if we already have zipcodes and locations, return dataframe (else return all 3)
    if has_zipcodes:
        return df
    
    return df, shortened_zipcodes, shortened_locations

### HARRIS TEETER
def ht_locations(zipcodes, sleep_time = 2, driver_wait = 20):
    '''
    Description:
        Identifies unique Harris Teeters in the Arlington area and returns their location and the zipcode connected to that store
    
    Inputs:
        zipcodes: list of Arlington zipcodes
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        
    Outputs:
        ht_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Harris Teeter
        unique_info: array of store addresses
    '''
    # create Harris Teeter driver, maximizing window
    ht_driver = webdriver.Chrome(ChromeDriverManager().install())
    ht_driver.maximize_window()

    # create empty lists for store locations and distances
    info = []; ht_distance = []

    # loop over each zipcode in Arlington and find the top store - remove any duplicates
    for j, zipcode in enumerate(zipcodes):
        # visit Harris Teeter website
        ht_driver.get("https://www.harristeeter.com/order-online/expresslane-groceries")

        # enter zip code
        wait = WebDriverWait(ht_driver, driver_wait)
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/app-root/app-groceries/section/div[1]/div/div/div[2]/pickup-text-component/div/form/div/mat-form-field/div/div[1]/div/input')))
        enter_zip.send_keys(str(zipcode))

        # accept cookies
        if check_exists_by_xpath(ht_driver, '//footer/div[4]/div[2]/div/button'):
            x_button_2 = ht_driver.find_element_by_xpath('//footer/div[4]/div[2]/div/button')
            x_button_2.click()
        
        # send zip code
        submit_zip = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'btn')))
        submit_zip.click()

        # Close "cannot determine location" message (might be specific to my computer)
        skip_loc_set = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[3]/div[2]/div/mat-dialog-container/material-confirm-prompt/div/div[2]/button')))
        skip_loc_set.click()

        # extract store location information
        element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/app-root/order-online-groceries-store-locator/div/store-locator-component/section/div[1]/div/div[1]/div[1]/div[2]/div/div[1]/div[1]/p')))
        info.append(element.text)

        # extract distance information
        element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/app-root/order-online-groceries-store-locator/div/store-locator-component/section/div[1]/div/div[1]/div[1]/div[2]/div/div[1]/div[2]/span')))
        ht_distance.append(element.text)

    # close driver
    ht_driver.quit()

    # remove any duplicates
    unique_info, ht_idxs = np.unique(info, return_index = True)
    ht_zipcodes = zipcodes[ht_idxs]
    unique_ht_distance = np.array(ht_distance)[ht_idxs]
    
    # clean location information
    for i, loc in enumerate(unique_info):
        unique_info[i] = ", ".join(loc.split("\n")[:2])
    
    return ht_zipcodes, unique_info

def ht_driver(ht_zipcodes, ht_foods, sleep_time = 2, driver_wait = 20, standard = True):
    '''
    Description:
        Extracts food item information for unique Harris Teeter locations in list of zipcodes
    
    Inputs:
        ht_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Harris TEeter
        ht_foods:
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        standard: Boolean, True if using "Universal" basket of goods
        
    Outputs:
        ht_items_by_zip: 2D array of food items by store (contains other information)
    '''
    # loop over zipcodes
    ht_items_by_zip = [] 
    for zipcode in ht_zipcodes:

        # create Harris Teeter Driver with maximized window, create empty list of items to be built out for each store
        ht_driver = webdriver.Chrome(ChromeDriverManager().install())
        ht_driver.maximize_window()

        # visit Harris Teeter website
        ht_driver.get("https://www.harristeeter.com/order-online/expresslane-groceries")

        # 
        if zipcode == ht_zipcodes[0]:
            wait = WebDriverWait(ht_driver, driver_wait)
            accept_cookies = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/footer/div[4]/div[2]/div/button')))
            accept_cookies.click()

        # enter zipcodes
        wait = WebDriverWait(ht_driver, driver_wait)
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/app-root/app-groceries/section/div[1]/div/div/div[2]/pickup-text-component/div/form/div/mat-form-field/div/div[1]/div/input')))
        enter_zip.send_keys(str(zipcode))

        # submit zipcode
        submit_zip = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'btn')))
        submit_zip.click()

        # Close "cannot determine location" message (might be specific to my computer)
        skip_loc_set = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[3]/div[2]/div/mat-dialog-container/material-confirm-prompt/div/div[2]/button')))
        skip_loc_set.click()

        # go to the top store (the unique store associated with that zipcode)
        go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/app-root/order-online-groceries-store-locator/div/store-locator-component/section/div[1]/div/div[1]/div[1]/div[2]/div/div[1]/div[2]/a[2]')))
        go_top_store.send_keys("\n")

        # loop over staples and store information in list
        ht_items = []
        for food in ht_foods:
            # select search bar and enter staple
            time.sleep(sleep_time)
            search_bar = ht_driver.find_element_by_id("searchStr-mobile")
            search_bar.clear()
            search_bar.send_keys(food)

            # search
            enter_food = ht_driver.find_element_by_xpath('//hts-search-product/div/button')
            enter_food.click()

            # if standard basket
            if standard:
                # sort this particular item by unit price
                if food == ht_foods[0]:
                    time.sleep(sleep_time)
                    drop_down = ht_driver.find_element_by_id("dropdownMenuButton")
                    drop_down.click()
                    unit_price = ht_driver.find_element_by_xpath("/html/body/app-root/div/hts-layout/span/hts-search/div/section/div/div[2]/div[1]/div[2]/div[1]/div/div/a[4]")
                    unit_price.click()

                # sort these items by price
                if (food == ht_foods[1]) or (food == ht_foods[11]) or (food == ht_foods[12]):
                    if zipcode != ht_zipcodes[5]:
                        time.sleep(sleep_time)
                        drop_down = ht_driver.find_element_by_id("dropdownMenuButton")
                        drop_down.click()
                        price = ht_driver.find_element_by_xpath("/html/body/app-root/div/hts-layout/span/hts-search/div/section/div/div[2]/div[1]/div[2]/div[1]/div/div/a[3]")
                        price.click()

                # FIX THIS!!!??? CHECK THIS PART
                if (food == ht_foods[-2]) and (zipcode == ht_zipcodes[2]):
                    time.sleep(sleep_time)
                    element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/app-root/div/hts-layout/span/hts-search/div/section/div/div[2]/div[2]/ul/hts-product-info[2]/li/span/a[2]/span[2]')))
                    ht_items.append(element.text)

                else:
                    # extract information on food item and add to list
                    time.sleep(sleep_time)
                    element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'forlist-view')))
                    ht_items.append(element.text)

            else:
                # extract information on food item and add to list
                time.sleep(sleep_time)
                element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'forlist-view')))
                ht_items.append(element.text)
        ht_items_by_zip.append(ht_items)

        # close driver
        ht_driver.quit()

    # convert from list to array
    ht_items_by_zip = np.array(ht_items_by_zip)
    return ht_items_by_zip

def ht_price_item_other(ht_items_by_zip):
    '''
    Description:
        Extracts food item, price and other information from 2D array of information passed as input
    
    Inputs:
        ht_items_by_zip: 2D array of food items by store (contains other information)

    Outputs:
        prices_ht: 2D array of prices for each item at each store
        cleaned_items_ht: 2D array of products at each store
        other_info_ht: 2D array of additional product information
    '''
    # clean strings and separate price, items, and other information
    cleaned_items_ht = np.array([np.array([ht_items_by_zip[j][i].split("\n")[0] for i in range(len(ht_items_by_zip[j]))]) for j in range(len(ht_items_by_zip))])
    prices_ht = np.array([np.array([float(ht_items_by_zip[j][i].split("\n")[2].split(" ")[0].replace("$", "").replace("/lb", "")) for i in range(len(ht_items_by_zip[j]))]) for j in range(len(ht_items_by_zip))])
    other_info_ht = np.array([np.array([ht_items_by_zip[j][i].split("\n")[1] for i in range(len(ht_items_by_zip[j]))]) for j in range(len(ht_items_by_zip))])
    return prices_ht, cleaned_items_ht, other_info_ht

def ht(foods, zipcodes, standard = True, has_zipcodes = False, locs = []):
    '''
    Description:
        Runs combination of functions to go from a list of zipcodes and food items to final dataframe for Harris Teeter
    
    Inputs:
        foods: array of food items
        zipcodes: list of Arlington zipcodes
        standard: Boolean, True if using "Universal" basket of goods
    
    Outputs:
        df: dataframe with price, info, etc.
    '''
    # if shortened zipcodes list passed in
    if has_zipcodes:
        shortened_zipcodes = zipcodes
        shortened_locations = locs
    
    # get shortened list of zipcodes if not provided
    else:
        shortened_zipcodes, shortened_locations = ht_locations(zipcodes)
    
    items_by_zip = ht_driver(shortened_zipcodes, ht_foods = foods, standard = standard)
    prices_ht, cleaned_items_ht, other_info_ht = ht_price_item_other(items_by_zip)
    df = make_df(prices_ht, cleaned_items_ht, other_info_ht, shortened_locations, "Harris Teeter")
    
    # if we already have zipcodes and locations, return dataframe (else return all 3)
    if has_zipcodes:
        return df
    
    return df, shortened_zipcodes, shortened_locations

### SAFEWAY
def sw_locations(zipcodes, sleep_time = 2, driver_wait = 20):
    '''
    Description:
        Identify unique Safeways in the Arlington Area
    
    Inputs:
        zipcodes: list of Arlington zipcodes
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
    
    Outputs:
        sw_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Safeway
        unique_sw_locations:
    '''
    # create Safeway driver and maximize window
    sw_driver = webdriver.Chrome(ChromeDriverManager().install())
    sw_driver.get("https://www.safeway.com/")
    sw_driver.maximize_window()

    # click to change zipcode
    wait = WebDriverWait(sw_driver, driver_wait)
    change_zip = wait.until(EC.element_to_be_clickable((By.ID, 'openFulfillmentModalButton')))
    change_zip.click()

    # loop over zipcodes
    sw_info = []
    sw_distance = []
    for j, zipcode in enumerate(zipcodes):
        # enter zipcode and submit
        wait = WebDriverWait(sw_driver, driver_wait)
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[2]/div/div/div[3]/div/div/div/div/div[2]/store-fulfillment-modal-unified/div/div/div/div[2]/store-fulfillment-tabs/div/div[1]/input')))
        enter_zip.clear()
        time.sleep(sleep_time)
        enter_zip.send_keys(str(zipcode), '\n')

        # extract location information on Safeway
        time.sleep(sleep_time)
        element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[2]/div/div/div[3]/div/div/div/div/div[2]/store-fulfillment-modal-unified/div/div/div/div[2]/store-fulfillment-tabs/div/div[2]/div/div[1]/div/div/div[1]//div[1]')))
        sw_info.append(element.text)

        # extract distance information on Safeway
        element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[2]/div/div/div[3]/div/div/div/div/div[2]/store-fulfillment-modal-unified/div/div/div/div[2]/store-fulfillment-tabs/div/div[2]/div/div[1]/div/div/div[1]/store-card/div[2]/div/p')))
        sw_distance.append(element.text)

    # close driver
    sw_driver.quit()

    # extracts unique Safeway locations and zipcodes with unique safeways
    unique_sw_locations, sw_idx = np.unique(sw_info, return_index = True)
    sw_zipcodes = zipcodes[sw_idx]
    
    #
    for i, loc in enumerate(unique_sw_locations):
        unique_sw_locations[i] = loc.replace("\n", ", ")
    
    return sw_zipcodes, unique_sw_locations

def sw_driver(sw_zipcodes, sw_foods, sleep_time = 2, driver_wait = 20, standard = True):
    '''
    Description:
        Extract item information for specific stores for searched food items
    
    Inputs:
        sw_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Safeway
        sw_foods:
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        standard: Boolean, True if using "Universal" basket of goods
        
    Outputs:
        sw_items_by_zip: 2D array of food items by store (contains other information)
    '''
    # struggles on the last 2 unique Safeways in Arlington area - makes exception and work!
    # create Safeway driver and maximize window
    sw_driver = webdriver.Chrome(ChromeDriverManager().install())
    sw_driver.maximize_window()

    # loop over all of the zipcodes for which there is a unique closest safeway
    sw_items_by_zip = []
    for k, zipcode in enumerate(sw_zipcodes):
        # go to Saveway website
        sw_driver.get("https://www.safeway.com/")

        # go to change zipcode
        wait = WebDriverWait(sw_driver, driver_wait)
        change_zip = wait.until(EC.element_to_be_clickable((By.ID, 'openFulfillmentModalButton')))
        change_zip.click()

        # change the zipcode
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[2]/div/div/div[3]/div/div/div/div/div[2]/store-fulfillment-modal-unified/div/div/div/div[2]/store-fulfillment-tabs/div/div[1]/input')))
        enter_zip.clear()
        time.sleep(sleep_time)
        enter_zip.send_keys(str(zipcode), '\n')

        # go to the top store for that zipcode (exception for last two stores - not sure why this is an issue)
        time.sleep(sleep_time)
        if k < 6:
            go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[2]/div/div/div[3]/div/div/div/div/div[2]/store-fulfillment-modal-unified/div/div/div/div[2]/store-fulfillment-tabs/div/div[2]/div/div[1]/div/div/div[1]/store-card/div[2]/div/a')))
            go_top_store.click()
        else:
            # select #2 store
            go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[2]/div/div/div[3]/div/div/div/div/div[2]/store-fulfillment-modal-unified/div/div/div/div[2]/store-fulfillment-tabs/div/div[2]/div/div[1]/div/div/div[2]/store-card/div[2]/div/a')))
            go_top_store.click()

        # loop over all the staple items
        sw_items = []
        time.sleep(sleep_time)
        for food in sw_foods:
            # search for food
            time.sleep(sleep_time)
            search_bar = sw_driver.find_element_by_id("skip-main-content")
            search_bar.clear()
            search_bar.send_keys(food, "\n")

            # if standard basket
            if standard:
                # 
                if (food == sw_foods[2]) or (food == sw_foods[3]):
                    time.sleep(sleep_time)
                    sort = wait.until(EC.element_to_be_clickable((By.XPATH, "//sort-by/div/div/button")))
                    sort.click()
                    price = wait.until(EC.element_to_be_clickable((By.XPATH, "//search-sort/sort-by/div/div/ul/li[2]/a")))
                    price.click()

            # extract information on staple and append to list
            time.sleep(sleep_time)
            element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'container')))
            sw_items.append(element.text)
        sw_items_by_zip.append(sw_items)

    # close driver
    sw_driver.quit()

    # convert from list to array
    sw_items_by_zip = np.array(sw_items_by_zip)
    return sw_items_by_zip

def sw_price_item_other(sw_items_by_zip):
    '''
    Description:
        Extracts food item, price and other information from 2D array of information passed as input
    
    Inputs:
        sw_items_by_zip: 2D array of food items by store (contains other information)
    
    Outputs:
        prices_sw: 2D array of prices for each item at each store
        cleaned_items_sw: 2D array of products at each store
        other_info_sw: 2D array of additional product information
    '''
    # clean strings and separate price, items, and other information
    cleaned_safeway = np.array([np.array([sw_items_by_zip[j][i][19:] if sw_items_by_zip[j][i][0] == "a" else sw_items_by_zip[j][i][11:] for i in range(len(sw_items_by_zip[j]))]) for j in range(len(sw_items_by_zip))])
    cleaned_items_sw = np.array([np.array([cleaned_safeway[j][i].split("\n")[-2] for i in range(len(cleaned_safeway[j]))]) for j in range(len(cleaned_safeway))])
    prices_sw = np.array([np.array([float(cleaned_safeway[j][i].split("\n")[0].split(" ")[0].replace("$", "")) for i in range(len(cleaned_safeway[j]))]) for j in range(len(cleaned_safeway))])
    other_info_sw = np.array([np.array([cleaned_safeway[j][i].split("\n")[-1] for i in range(len(cleaned_safeway[j]))]) for j in range(len(cleaned_safeway))])
    return prices_sw, cleaned_items_sw, other_info_sw

def sw(foods, zipcodes, standard = True, has_zipcodes = False, locs = []):
    '''
    Description:
        Runs combination of functions to go from a list of zipcodes and food items to final dataframe for Safeway
    
    Inputs:
        foods: array of food items
        zipcodes: list of Arlington zipcodes
        standard: Boolean, True if using "Universal" basket of goods
    
    Outputs:
        df: dataframe with price, info, etc.
    '''
    # if shortened zipcodes list passed in
    if has_zipcodes:
        shortened_zipcodes = zipcodes
        shortened_locations = locs
    
    # get shortened list of zipcodes if not provided
    else:
        shortened_zipcodes, shortened_locations = sw_locations(zipcodes)
    
    items_by_zip = sw_driver(shortened_zipcodes, sw_foods = foods, standard = standard)
    prices_sw, cleaned_items_sw, other_info_sw = sw_price_item_other(items_by_zip)
    df = make_df(prices_sw, cleaned_items_sw, other_info_sw, shortened_locations, "Safeway")
    
    # if we already have zipcodes and locations, return dataframe (else return all 3)
    if has_zipcodes:
        return df
    
    return df, shortened_zipcodes, shortened_locations

### ALDI
# can try out 10 mile radius OR whatever radisu is desired
def aldi_locations(zipcodes, sleep_time = 2, driver_wait = 20, dist = 5):
    '''
    Description:
        Identify unique Aldi in the Arlington Area
    
    Inputs:
        zipcodes: list of Arlington zipcodes
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        
    Outputs:
        aldi_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Aldi
        unique_aldi_locs: array of unique Aldi locations close to Arlington
    '''
    # Create ALDI driver and visit website
    aldi_driver = webdriver.Chrome(ChromeDriverManager().install())
    aldi_driver.get("https://shop.aldi.us/store/aldi/storefront/?utm_source=aldi_outlink&utm_medium=google.com|none|search|none")

    # click on pickup option to locate stores
    wait = WebDriverWait(aldi_driver, driver_wait)
    click_pickup = wait.until(EC.element_to_be_clickable((By.ID, 'service-type-button-pickup')))
    click_pickup.click()
    
    # create empty list for ALDI locations and loop over zipcodes
    aldi_locs = []
    zips_to_return = []
    for i, zipcode in enumerate(zipcodes):
        # if starting the loop then take an additional step send a single space (not sure why a response is needed before removing the zipcode placeholder)
        if i == 0:
            send_zip = wait.until(EC.element_to_be_clickable((By.ID, 'locationInput')))
            send_zip.send_keys(" ")

        # send zipcode and hit enter
        send_zip = wait.until(EC.element_to_be_clickable((By.ID, 'locationInput')))
        send_zip.clear()
        send_zip.send_keys(str(zipcode), "\n")
        
        # set counter to 1, correspind
        count = 1
        time.sleep(sleep_time)
        
        #
        while True:
            # FIX LATER - GETTING ILLONOIS STORE LOCATIONS FOR THE FINAL STORE
            if zipcode == zipcodes[-1]:
                break
            
            #
            element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/header/div/div/div[7]/div[2]/div/div/div[1]/div[4]/button[{}]/div[2]'.format(count))))

            #
            if float(element.text.split("\n")[-1].split(" ")[0]) > dist:
                break

            # check if store already in list
            if ", ".join(element.text.split("\n")[:2]) not in aldi_locs:
                aldi_locs.append(", ".join(element.text.split("\n")[:2]))
                # check if zipcode was already added to list
                if zipcode not in zips_to_return:
                    zips_to_return.append(zipcode)

            # increment counter by 1
            count += 1

        time.sleep(sleep_time)

    # close driver
    aldi_driver.quit()

    # get unique locations and corresponding zipcodes
    unique_aldi_locs = np.unique(np.array([", ".join(aldi_locs[i].split("\n")[:2]) for i in range(len(aldi_locs))]))
    return zips_to_return, unique_aldi_locs


# NEED TO ALLOW FOR MULTIPLE SELECTIONS FOR ZIP
def aldi_driver(locations, aldi_foods, sleep_time = 2, driver_wait = 20, standard = True):
    '''
    Description:
        Extract item information for specific stores for searched food items
    
    Inputs:
        aldi_zipcodes: array of zipcode integers, shortened to zipcodes with a unique closest Aldi
        aldi_foods: array of food items to be searched for
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        standard: Boolean, True if using "Universal" basket of goods
        
    Outputs:
        aldi_items_by_zip: 2D array of food items by store (contains other information)
    '''
    # loop over all ALDI's zipcodes with unique stores (goal: find all ALDIs in reasonable distance from Arlington)
    aldi_items_by_zip = []
    for k, location in enumerate(locations):
        # create driver and go to website
        aldi_driver = webdriver.Chrome(ChromeDriverManager().install())
        aldi_driver.get("https://shop.aldi.us/store/aldi/storefront/?utm_source=aldi_outlink&utm_medium=google.com|none|search|none")

        # clicks the pickip option which allows you to select stores
        wait = WebDriverWait(aldi_driver, driver_wait)
        click_pickup = wait.until(EC.element_to_be_clickable((By.ID, 'service-type-button-pickup')))
        click_pickup.click()

        # deletes then sends zipcode (2x) - wasn't functional when only 1x
        for m in range(2):
            send_zip = wait.until(EC.element_to_be_clickable((By.ID, 'locationInput')))
            send_zip.clear()
            send_zip.send_keys(location, "\n")
            send_zip.send_keys("\n")
        go = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/header/div/div/div[7]/div[2]/div/div/div[1]/div[3]/div/div[2]/div[2]')))
        go.click()

        # go to the top store for that zipcode
        time.sleep(sleep_time)
        
        #
        if check_intercepted_by_xpath(aldi_driver, "/html/body/div[1]/div/header/div/div/div[7]/div[2]/div/div/div[1]/div[4]/button[1]"):
            send_zip = wait.until(EC.element_to_be_clickable((By.ID, 'locationInput')))
            send_zip.send_keys("\n")

        go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div/header/div/div/div[7]/div[2]/div/div/div[1]/div[4]/button[1]")))
        go_top_store.click()

        # start looping other staple items after sleep
        time.sleep(sleep_time)
        aldi_items = []
        for n, food in enumerate(aldi_foods):
            # send driver to search for food item - had to do this a bit differently due to some complications
            aldi_driver.get('https://shop.aldi.us/store/aldi/search_v3/{}'.format(food))
            
            # if standard basket
            if standard:
                # set of steps for searching specifically for 12 eggs... (gives cheapest option - likely other, easier ways to do this)
#                 if (n == 0):
#                     time.sleep(sleep_time)
#                     drop_down_brands = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[1]/div[2]/button")
#                     drop_down_brands.click()
#                     friendly = aldi_driver.find_element_by_id("16328")
#                     friendly.click()
#                     apply = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[1]/div[2]/div/div[2]/button[2]")
#                     apply.click()
                
                #
                if (n == 1):
                    time.sleep(sleep_time)
                    drop_down_brands = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[1]/div[2]/button")
                    drop_down_brands.click()
                    goldhen = aldi_driver.find_element_by_id("231184")
                    goldhen.click()
                    apply = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[1]/div[2]/div/div[2]/button[2]")
                    apply.click()
                    sort_by = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[2]/button")
                    sort_by.click()
                    price = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[2]/div/button[2]")
                    price.click()

                #
                if (n == 8):
                    time.sleep(sleep_time)
                    drop_down_brands = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[1]/div[2]/button")
                    drop_down_brands.click()
                    countryside = aldi_driver.find_element_by_id("37205")
                    countryside.click()
                    apply = aldi_driver.find_element_by_xpath("/html/body/div[1]/div/div/div/div/div/div/div[1]/div/div/div[1]/div[1]/div[2]/div/div[2]/button[2]")
                    apply.click()

            # select top food item under that search
            time.sleep(sleep_time)
            pic = wait.until(EC.element_to_be_clickable((By.XPATH, '//a[@class="css-er4k5d"]')))
            pic.click()

            # extract information on that food item
            time.sleep(2*sleep_time)
            element = wait.until(EC.element_to_be_clickable((By.XPATH, '//div[@class="col-md-5 itemModalHeader"]/div')))
            aldi_items.append(element.text)

            # close out of popup for that item - returning to search results
            close = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[5]/div/div/div/div[2]/div/div/div/div/div/div/div[1]/button')))
            close.click()

        # add all items from a store to list and close driver
        aldi_items_by_zip.append(aldi_items)
        aldi_driver.quit()

    # convert from list to array
    aldi_items_by_zip = np.array(aldi_items_by_zip)
    return aldi_items_by_zip

def aldi_price_item_other(aldi_items_by_zip):
    '''
    Description:
        Extracts food item, price and other information from 2D array of information passed as input
    
    Inputs:
        aldi_items_by_zip: 2D array of food items by store (contains other information)
        
    Outputs:
        prices_aldi: 2D array of prices for each item at each store
        cleaned_items_aldi: 2D array of products at each store
        other_info_aldi: 2D array of additional product information
    '''
    # clean strings and separate price, items, and other information
    cleaned_aldi = np.array([np.array([aldi_items_by_zip[j][i].replace("\nFREE PICKUP", "").replace("\nCurrent price:", "") for i in range(len(aldi_items_by_zip[j]))]) for j in range(len(aldi_items_by_zip))])

    # on occasion, price and other information is not completely scraped (usually for later stores)
    # sets later stores information to 0th store (usually the same or very close anyway)
    # THIS MAY BE A BUGGY PART OF CODE
    for i in range(len(cleaned_aldi) - 1):
        for j in range(len(cleaned_aldi[0])):
            if (len(cleaned_aldi[i + 1][j].split('\n')) == 1):
                cleaned_aldi[i + 1][j] = cleaned_aldi[0][j]

    # finish cleaning strings and separating information
    cleaned_items_aldi = np.array([np.array([cleaned_aldi[j][i].split("\n")[0] for i in range(len(cleaned_aldi[j]))]) for j in range(len(cleaned_aldi))])
    
    prices_aldi = np.array([np.array([float(re.findall("\d+\.\d*", cleaned_aldi[j][i])[0]) for i in range(len(cleaned_aldi[j]))]) for j in range(len(cleaned_aldi))])
    
#     prices_aldi = np.array([np.array([float(cleaned_aldi[j][i].split("Current price:\n")[-1].split("\n")[0].split(" ")[0].replace("$",""))  for i in range(len(cleaned_aldi[j]))]) for j in range(len(cleaned_aldi))])
    
#     prices_aldi = np.array([np.array([float(cleaned_aldi[j][i].split("\n")[2].split("$")[1].split(" ")[0]) for i in range(len(cleaned_aldi[j]))]) for j in range(len(cleaned_aldi))])
    other_info_aldi = np.array([np.array([cleaned_aldi[j][i].split("\n")[1] for i in range(len(cleaned_aldi[j]))]) for j in range(len(cleaned_aldi))])
    return prices_aldi, cleaned_items_aldi, other_info_aldi

def aldi(foods, zipcodes, standard = True, has_zipcodes = False, locs = []):
    '''
    Description:
        Runs combination of functions to go from a list of zipcodes and food items to final dataframe for Aldi
    
    Inputs:
        foods: array of food items
        zipcodes: list of Arlington zipcodes
        standard: Boolean, True if using "Universal" basket of goods
        
    Outputs:
        df: dataframe with price, info, etc.
    '''
    # if shortened zipcodes list passed in
    if has_zipcodes:
        shortened_zipcodes = zipcodes
        shortened_locations = locs
    
    # get shortened list of zipcodes if not provided
    else:
        shortened_zipcodes, shortened_locations = aldi_locations(zipcodes)
    
    items_by_zip = aldi_driver(shortened_locations, aldi_foods = foods, standard = standard)
    prices_aldi, cleaned_items_aldi, other_info_aldi = aldi_price_item_other(items_by_zip)
    df = make_df(prices_aldi, cleaned_items_aldi, other_info_aldi, shortened_locations, "Aldi")
    
    # if we already have zipcodes and locations, return dataframe (else return all 3)
    if has_zipcodes:
        return df
    
    return df, shortened_zipcodes, shortened_locations

### TARGET
def target_locations(zipcodes, sleep_time = 2, driver_wait = 20):
    '''
    Description:
        Identify unique Targets in the Arlington Area
    
    Inputs:
        zipcodes:
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        
    Outputs:
        target_zipcodes: array of zipcodes shortened to identify unique Targets
        unique_target_locs: array of Target locations
    '''
    # Create Target driver and visit website
    target_driver = webdriver.Chrome(ChromeDriverManager().install())
    target_driver.get("https://www.target.com/c/grocery/-/N-5xt1a")

    # select store option to begin passing in zipcodes
    wait = WebDriverWait(target_driver, driver_wait)
    store = wait.until(EC.element_to_be_clickable((By.ID, 'storeId-utilityNavBtn')))
    store.click()

    # loop over zipcodes to collect information on store location
    target_info = []
    for zipcode in zipcodes:
        # click on option to edit zipcode
        edit = wait.until(EC.element_to_be_clickable((By.ID, 'zipOrCityState')))
        edit.click()

        # delete zipcode then send new zipcode (not sure why enter_zip.clear() did not work)
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[9]/div/div/div/div/div[1]/div/div[3]/div[1]/div/input')))
        enter_zip.send_keys('\b\b\b\b\b')
        enter_zip.send_keys(str(zipcode), "\n")

        # extract store location information
        element = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[9]/div/div/div/div/div[3]')))
        target_info.append(element.text)

    # close driver
    target_driver.quit()

    # get unique locations and corresponding zipcodes
    unique_target_locs, target_idx = np.unique(target_info, return_index = True)
    target_zipcodes = zipcodes[target_idx]
    
    # clean location information
    for i, loc in enumerate(unique_target_locs):
        unique_target_locs[i] = loc.split("\n")[1]
    
    #
    return target_zipcodes, unique_target_locs

def target_driver(target_zipcodes, target_foods, sleep_time = 2, driver_wait = 20, standard = True):
    '''
    Description:
        Extract item information for specific stores for searched food items
    
    Inputs:
        target_zipcodes: array of zipcodes to enter to identify closest stores
        target_foods: array of food items to search for on website
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        standard: Boolean, True if using "Universal" basket of goods
        
    Outputs:
        target_items_by_zip: 2D array of food items by store (contains other information)
    '''
    # start driver and visit Target side
    target_driver = webdriver.Chrome(ChromeDriverManager().install())
    target_driver.maximize_window()
    target_driver.get("https://www.target.com/c/grocery/-/N-5xt1a")

    # loop over all unique Targets
    target_items_by_zip = []
    for k, zipcode in enumerate(target_zipcodes):
        # click on option to select a store
        wait = WebDriverWait(target_driver, driver_wait)
        store = wait.until(EC.element_to_be_clickable((By.ID, 'storeId-utilityNavBtn')))
        store.click()

        # edit and then add zipcode
        edit = wait.until(EC.element_to_be_clickable((By.ID, 'zipOrCityState')))
        edit.click()

        # erase old zipcode and send new one
        enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[9]/div/div/div/div/div[1]/div/div[3]/div[1]/div/input')))
        enter_zip.send_keys('\b\b\b\b\b')
        enter_zip.send_keys(str(zipcode), "\n")

        # select the top store
        go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[9]/div/div/div/div/div[3]/div[2]/div[1]/button')))
        go_top_store.click()

        # loop over staples at store
        targ_info = []
        for food in target_foods:
            # selects next closest beef option for this particular store
            if (food == "Ground Beef 1lb 80") and (k == 4):
                food = "Ground Beef 1lb 73"

            # search for food item
            target_driver.get("https://www.target.com/s?searchTerm={}".format(food))
            
            # checks if search item is tomato sauce to sort by price
            if (food == "Tomato Sauce 8oz"):
                sort = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div/div[4]/div[4]/div[2]/div/div[2]/div[3]/div[1]/div[2]/div[2]/div/div[2]/button")))
                sort.click()
                by_price = wait.until(EC.element_to_be_clickable((By.XPATH, "//li[3]/a/div/div")))
                by_price.click()
                time.sleep(sleep_time)
            
            # checks if search item is NOT plantains (these are in different section for some reason...)
            if (food != "Platano"):
            # extract slightly cleaned pricing info
#                 info = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div/div[4]/div[4]/div[2]/div/div[2]/div[3]/div/ul/li[1]/div/div[2]/div/div/div')))
                info = wait.until(EC.element_to_be_clickable((By.XPATH, '//*[@id="mainContainer"]/div[4]/div[2]/div/div[2]/div[3]/div/ul/li[1]/div/div[2]/div')))
#/html/body/div[1]/div/div[4]/div[4]/div[2]/div/div[2]/div[3]/div/ul/li[1]/div/div[2]/div/div/div
            
            # if food item is plantain - grab info from bottom of webpage
            else:
                info = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div/div[4]/div[4]/div[2]/div/div[1]/div[3]/div[2]/div/ul/li[1]/div/div[2]/div/div/div")))
                
            #
            cleaned_info = "".join(re.split('(.\d\d)', info.text)[:-1])
            targ_info.append(cleaned_info)

        # append list of items to list and close driver
        target_items_by_zip.append(targ_info)
    target_driver.quit()
    
    # convert from list to array
    target_items_by_zip = np.array(target_items_by_zip)
    return target_items_by_zip

def target_price_item_other(target_items_by_zip):
    '''
    Description:
        Extracts price, item info, and other information from the 2D array of information passed in
    
    Inputs:
        target_items_by_zip: 2D array of food items by store (contains other information)
    
    Outputs:
        prices_target: 2D array of prices for each item at each store
        cleaned_items_target: 2D array of products at each store
        other_info_target: 2D array of additional product information
    '''
    # clean strings and separate price, items, and other information
    cleaned_target = np.array([np.array([target_items_by_zip[j][i] for i in range(len(target_items_by_zip[j]))]) for j in range(len(target_items_by_zip))])
    cleaned_items_target = np.array([np.array([cleaned_target[j][i].split("\n")[0] for i in range(len(cleaned_target[j]))]) for j in range(len(cleaned_target))])
    
    prices_target = np.array([np.array([float(re.sub("[^0-9\.]", "", cleaned_target[j][i].split("\n$")[-1].split(" ")[0])) for i in range(len(cleaned_target[j]))]) for j in range(len(cleaned_target))])
    other_info_target = np.array([np.array(["" for i in range(len(cleaned_target[j]))]) for j in range(len(cleaned_target))])
    return prices_target, cleaned_items_target, other_info_target

def target(foods, zipcodes, standard = True, has_zipcodes = False, locs = []):
    '''
    Description:
        Runs combination of functions to go from a list of zipcodes and food items to final dataframe for Target
    
    Inputs:
        foods: array of food items
        zipcodes: list of Arlington zipcodes
        standard: Boolean, True if using "Universal" basket of goods
    
    Outputs:
        df: dataframe with price, info, etc.
    '''
    # if shortened zipcodes list passed in
    if has_zipcodes:
        shortened_zipcodes = zipcodes
        shortened_locations = locs
    
    # get shortened list of zipcodes if not provided
    else:
        shortened_zipcodes, shortened_locations = target_locations(zipcodes)
    
    items_by_zip = target_driver(shortened_zipcodes, target_foods = foods, standard = standard)
    prices_target, cleaned_items_target, other_info_target = target_price_item_other(items_by_zip)
    df = make_df(prices_target, cleaned_items_target, other_info_target, shortened_locations, "Target")
    
    # if we already have zipcodes and locations, return dataframe (else return all 3)
    if has_zipcodes:
        return df
    
    return df, shortened_zipcodes, shortened_locations

### WHOLE FOODS MARKET
def wfm_locations(zipcodes, sleep_time = 2, driver_wait = 20):
    '''
    Description:
        Identify unique Whole Foods in the Arlington Area

    Inputs:
        zipcodes: list of Arlington zipcodes
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        
   Outputs:
       wfm_zipcodes: array of zipcodes shortened to identify unique Whole Foods
       unique_wfm_locs: array of Whole Foods locations
    '''
    # starts driver and maximizes window
    wfm_driver = webdriver.Chrome(ChromeDriverManager().install())
    wfm_driver.maximize_window()

    # loops over all Arlington zipcodes to find local WFMs
    wfm_locations = []
    for zipcode in zipcodes:
        # goes to WFM site (this approach works although it isn't the most efficient - shouldn't need to revisit site)
        wfm_driver.get("https://www.wholefoodsmarket.com/stores")

        # select option to browse products to select store
        wait = WebDriverWait(wfm_driver, driver_wait)
        browse_pdts = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/main/header/div/header/nav/div[3]/ul/li[1]/a')))
        browse_pdts.click()

        # send zipcode to get to top stores
        send_zip = wait.until(EC.element_to_be_clickable((By.ID, 'pie-store-finder-modal-search-field')))
        send_zip.send_keys(str(zipcode))

        # extract information on closest store to that zipcode
        go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div/main/div[2]/div[4]/div/div/div/section/div/wfm-search-bar/div[2]/div/ul/li[1]')))
        wfm_locations.append(go_top_store.text)

    # close driver
    wfm_driver.quit()

    # identify all unique locations (removing zipcodes corresponding to: "no stores found")
    unique_wfm_locs, wfm_idx = np.unique(wfm_locations, return_index = True)
    wfm_zipcodes = zipcodes[wfm_idx][[0, 2, 3]] # 1 = No stores found...
    return wfm_zipcodes, unique_wfm_locs[[0, 2, 3]]

def wfm_driver(wfm_zipcodes, wfm_foods, sleep_time = 2, driver_wait = 20, standard = True, east_euro = False, west_af = False):
    '''
    Description:
        Extract item information for specific stores for searched food items
    
    Inputs:
        wfm_zipcodes: array of zipcodes shortened to identify unique Whole Foods
        wfm_foods: array of food items to search for
        sleep_time: integer, system sleep time between certain processes, default = 2
        driver_wait: integer, wait time for driver - default = 20
        standard: Boolean, True if using "Universal" basket of goods
    
    Outputs:
        wfm_items_by_zip: 2D array of food items by store (contains other information)
    '''
    # collect items for stores
    wfm_items_by_zip = []

    # loop over important zipcodes - these zipcodes correspond to unique stores
    for ii, zipcode in enumerate(wfm_zipcodes):
        # start up driver and visit WFM, maximizng window
        wfm_driver = webdriver.Chrome(ChromeDriverManager().install())
        wfm_driver.get("https://www.wholefoodsmarket.com/stores")
        wfm_driver.maximize_window()

        # go to browse products
        wait = WebDriverWait(wfm_driver, driver_wait)
        browse_pdts = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/main/header/div/header/nav/div[3]/ul/li[1]/a')))
        browse_pdts.click()

        # send zipcode information
        send_zip = wait.until(EC.element_to_be_clickable((By.ID, 'pie-store-finder-modal-search-field')))
        send_zip.send_keys(str(zipcode))

        # go to the top store for that zipcode
        go_top_store = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div/main/div[2]/div[4]/div/div/div/section/div/wfm-search-bar/div[2]/div/ul/li[1]')))
        go_top_store.click()

        # empty list to store information on food items for each iteration in loop below
        wfm_info = []

        # loop to search for all food items
        for food in wfm_foods:
            # search for food item
            search = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div/main/header/nav/div[3]/div[1]/div/div/form/input')))
            search.send_keys(food, '\n')
            
            # if standard basket
            if standard:
                # sort by price if item is one of these two exceptions within standard basket
                if (food == wfm_foods[1]) or (food == wfm_foods[12]):
                    time.sleep(sleep_time)
                    drop_down = wfm_driver.find_element_by_id("sort-dropdown-select")
                    drop_down.click()
                    price = wfm_driver.find_element_by_xpath("//select/option[2]")
                    price.click()
            
            # if Eastern European basket
            elif east_euro:
                # sort by price if exceptions
                if (food == wfm_foods[-4]):
                    time.sleep(sleep_time)
                    drop_down = wfm_driver.find_element_by_id("sort-dropdown-select")
                    drop_down.click()
                    price = wfm_driver.find_element_by_xpath("//select/option[1]")
                    price.click()
                    
                #
                elif (food == wfm_foods[-2]):
                    time.sleep(sleep_time)
                    drop_down = wfm_driver.find_element_by_id("sort-dropdown-select")
                    drop_down.click()
                    price = wfm_driver.find_element_by_xpath("//select/option[2]")
                    price.click()

            # if West African basket
            elif west_af:
                #
                if (food == wfm_foods[-2]):
                    time.sleep(sleep_time)
                    drop_down = wfm_driver.find_element_by_id("sort-dropdown-select")
                    drop_down.click()
                    price = wfm_driver.find_element_by_xpath("//select/option[2]")
                    price.click()
            
            # wait a bit then copy text on this food item, appending it to list
            time.sleep(sleep_time)
            element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'w-pie--product-tile__content')))
            wfm_info.append(element.text)

        # add items to the list and close driver
        wfm_items_by_zip.append(wfm_info)
        wfm_driver.quit()
    
    # convert from list to array
    wfm_items_by_zip = np.array(wfm_items_by_zip)
    return wfm_items_by_zip

def wfm_price_item_other(wfm_items_by_zip):
    '''
    Description:
        Extracts price, item info, and other information from the 2D array of information passed in
    
    Inputs:
        wfm_items_by_zip: 2D array of food items by store (contains other information)
    
    Outputs:
        prices_wfm: 2D array of prices for each item at each store
        cleaned_items_wfm: 2D array of products at each store
        other_info_wfm: 2D array of additional product information
    '''
    # clean strings and separate price, items, and other information
    cleaned_wfm = np.array([np.array([wfm_items_by_zip[j][i] for i in range(len(wfm_items_by_zip[j]))]) for j in range(len(wfm_items_by_zip))])

    cleaned_items_wfm = np.array([np.array([cleaned_wfm[j][i].split("\n")[-2] for i in range(len(cleaned_wfm[j]))]) for j in range(len(cleaned_wfm))])
    temp = np.array([np.array([cleaned_wfm[j][i].split("\n")[-3].split("/")[0].replace("$", "").split("Regular")[1] if cleaned_wfm[j][i].split("\n")[-1][:5] == "Prime" else cleaned_wfm[j][i].split("\n")[-1].split("/")[0].replace("$", "") for i in range(len(cleaned_wfm[j]))]) for j in range(len(cleaned_wfm))])
    prices_wfm = np.array([np.array([int(temp[j][i].replace("¢", ""))/100 if temp[j][i][2] == "¢" else float(temp[j][i]) for i in range(len(temp[j]))]) for j in range(len(temp))])
    other_info_wfm = np.array([np.array(["" for i in range(len(cleaned_wfm[j]))]) for j in range(len(cleaned_wfm))])
    return prices_wfm, cleaned_items_wfm, other_info_wfm

def wfm(foods, zipcodes, standard = True, east_euro = False, west_af = False, has_zipcodes = False, locs = []):
    '''
    Description:
        Runs combination of functions to go from a list of zipcodes and food items to final dataframe for Whole Foods
    
    Inputs:
        foods: array of food items
        zipcodes: list of Arlington zipcodes
        standard: Boolean, True if using "Universal" basket of goods
    
    Outputs:
        df: dataframe with price, info, etc.
    '''
    # if shortened zipcodes list passed in
    if has_zipcodes:
        shortened_zipcodes = zipcodes
        shortened_locations = locs
    
    # get shortened list of zipcodes if not provided
    else:
        shortened_zipcodes, shortened_locations = wfm_locations(zipcodes)
    items_by_zip = wfm_driver(shortened_zipcodes, wfm_foods = foods, standard = standard, east_euro = east_euro, west_af = west_af)
    prices_wfm, cleaned_items_wfm, other_info_wfm = wfm_price_item_other(items_by_zip)
    df = make_df(prices_wfm, cleaned_items_wfm, other_info_wfm, shortened_locations, "Whole Foods")
    
    # if we already have zipcodes and locations, return dataframe (else return all 3)
    if has_zipcodes:
        return df
    
    return df, shortened_zipcodes, shortened_locations


### WALMART
def create_walmart_driver():
    '''
    Description:
        Creates driver for walmart and waits so that user can close popups and select change pickup option
        
    Inputs:
        None
    
    Outputs:
        walmart_driver: selenium driver open to Walmart grocery page
    '''
    walmart_driver = webdriver.Chrome(ChromeDriverManager().install())
    walmart_driver.get("https://www.walmart.com/grocery")
    walmart_driver.maximize_window()
    return walmart_driver

#
def search_zips(walmart_driver, zipcodes, distance_threshold = 10, driver_wait = 20):
    '''
    Description:
        After user passes reCAPTCHA, closes the popup, and choose pickup option, this function parses over zipcodes for store locations
    
    Inputs:
        walmart_driver: selenium driver open to Walmart grocery page
        zipcodes: list of Arlington zipcodes
        distance_threshold: integer, max distance allowed to be store
        driver_wait: integer, wait time for driver - default = 20
    
    Outputs:
        unique_stores: array of unique store locations
    '''
    # creates wait object
    wait = WebDriverWait(walmart_driver, driver_wait)

    # loop over zipcodes provided by user
    walmart_store_loc = []
    for zipcode in zipcodes:
        # change zipcode
        change_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/section[2]/div/div[1]/div/div[1]/div/form/div/input')))
        change_zip.clear()
        change_zip.send_keys(str(zipcode), '\n')

        # loop over all stores where distance less than max, increments counter to pass with increasing distance from zipcode
        count = 1
        while True:
            # get distance information for store
            dist_info = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/section[2]/div/div[1]/div/div[1]/div/ul/li[{}]/label/div/div/div[2]'.format(count))))
            dist = float(dist_info.text.split(" ")[0])

            # if store within range, add information to list and incremetn counter
            if dist <= distance_threshold:
                info = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/section[2]/div/div[1]/div/div[1]/div/ul/li[{}]/label/div/div/div[1]/div[1]/section/div'.format(count))))
                walmart_store_loc.append(info.text.replace("\n", ", "))
                count += 1

            # if distance is greater than threshold break from while loop and check next zipcode
            else:
                break

    # close the store locations pop-up
    x_locations = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/header/button')))
    x_locations.click()
    
    # identify uniuqe locations in array
    walmart_store_loc_array = np.array(walmart_store_loc)
    unique_stores = np.unique(walmart_store_loc_array)
    return unique_stores

#
def walmart_driver_func(walmart_driver, unique_stores, driver_wait = 20, *args):
    '''
    Description:
        Searches for food items by first identifying stores, and next looping over food baskets
    
    Inputs:
        walmart_driver: selenium driver open to Walmart grocery page
        unique_stores:
        driver_wait: integer, wait time for driver - default = 20
        args: variable number of food baskets for Arlington
    
    Outputs:
        walmart_all_info: l
    '''
    # creates wait object
    wait = WebDriverWait(walmart_driver, driver_wait)
    
    # loop over all stores using zipcode of stores to identify them
    walmart_all_info = []
    for store in unique_stores:
        # select option to change pickup
        change_pickup = wait.until(EC.element_to_be_clickable((By.XPATH, '//div[1]/div[1]/div[1]/button')))
        change_pickup.click()

        # enter store zipcode
        change_zip = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/section[2]/div/div[1]/div/div[1]/div/form/div/input')))
        change_zip.clear()
        change_zip.send_keys(store[-5:], '\n')

        # NEED A BETTER WAY TO FIND THE STOER THAT ALIGNS WITH ZIPCODE
        # what if 2 stores in the same zipcode...
        click_top = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/section[2]/div/div[1]/div/div[1]/div/ul/li[1]/label/div/div')))
        click_top.click()

        # 
        click_continue = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/section[2]/div/div[2]/button')))
        click_continue.click()
        click_change = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[2]/section/div/button')))
        click_change.click()

        # loop over all of the cultural baskets
        walmart_store_info = []
        for basket in args:
            walmart_basket_info = []
            
            # loop over all of the food items in that basket
            for food in basket:
                # search for the food item and append information on that item to list
                search_store = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, 'Search__searchField___3eXaL')))
                search_store.send_keys(food, '\n')
                info = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div/div[1]/div/div/main/div[1]/table/tbody/tr/td[2]/div[1]/div[1]/div/div[2]')))
                walmart_basket_info.append(info.text)

                # erase the search
                x_search = wait.until(EC.element_to_be_clickable((By.XPATH, '//html/body/div/div[1]/div/div/header/div[2]/div[2]/form/button[1]')))
                x_search.click()
            
            # append list of foods to list that will store all foods for that store
            walmart_store_info.append(walmart_basket_info)
            
        # append list of foods for that store to list which will store all items
        walmart_all_info.append(walmart_store_info)

        # return to homepage
        go_groceries = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div/div[1]/div/div/header/div[2]/div[1]/nav/a')))
        go_groceries.click()

    # close driver
    walmart_driver.quit()
    return walmart_all_info

def walmart_dfs(walmart_all_stores_baskets, names, unique_stores):
    '''
    Description:
        Convert store and basket information into dataframes for each basket
    
    Inputs:
        walmart_all_stores_baskets: list of lists (by store) of lists (by basket)
        names: list of csv desired file names
        
    Outputs:
        May return error message alerting user to enter a valid list of names of a valid length
    '''
    # if list of names is the wrong length, return message below
    if len(names) != len(walmart_all_stores_baskets[0]):
        return "List of Names must be {} in length".format(len(walmart_all_stores_baskets[0]))
    
    try:
        # loop over all of the baskets and generate csvs with appropriate names
        for i in range(len(names)):
            # obtain all information for that basket across stores
            walmart_all_info = np.array([store_basket[i] for store_basket in walmart_all_stores_baskets])

            # trim down strings
            walmart_foods_new = np.array([np.array([walmart_all_info[j][i].replace("Sponsored\n", "").replace("\nFinal cost by weight", "") for i in range(len(walmart_all_info[j]))]) for j in range(len(walmart_all_info))])

            # create data columns
            walmart_info = np.array([np.array([walmart_foods_new[j][i].split("\n")[-1] for i in range(len(walmart_foods_new[j]))]) for j in range(len(walmart_foods_new))])
            walmart_prices = np.array([np.array([float(re.split( "\(|each|\s", walmart_foods_new[j][i].split("\n")[1])[0].replace("$", "")) for i in range(len(walmart_foods_new[j]))]) for j in range(len(walmart_foods_new))])
            lengths = np.array([np.array([len(re.split( "\(|each|\s", walmart_foods_new[j][i].split("\n")[1])) for i in range(len(walmart_foods_new[j]))]) for j in range(len(walmart_foods_new))])
            walmart_more_info = np.array([np.array(["" if lengths[j][i] == 1 else re.split( "\(|each", walmart_foods_new[j][i].split("\n")[1])[-1] for i in range(len(lengths[j]))]) for j in range(len(lengths))])

            # create dataframe with information above and convert to csv
            df = make_df(walmart_prices, walmart_info, walmart_more_info, unique_stores, "Walmart")
            df.to_csv('{}'.format(names[i]))

    # if list of names is not valid for some other reason, return message below
    except:
        return "Enter valid list of names"

def amazon(zipcodes, names, baskets, driver_wait = 20, sleep_time = 2):
    '''
    Description:
        Generates dataframes for each cultural basket at 22201 zipcode in Arlington (may need to test other zipcodes in the county to ensure that the pricing is consistent across the county)
    
    Inputs:
        zipcodes: list of Arlington zipcodes (for now we only use 22201)
        names: list of names for csv files generated by function - should be the same length as arguments passed for baskets
        baskets:
        driver_wait: integer, wait time for driver - default = 20
        sleep_time: integer, system sleep time between certain processes, default = 2
    
    Outputs:
        None
    '''
    # create Amazon driver and maximize window
    amazon_driver = webdriver.Chrome(ChromeDriverManager().install())
    amazon_driver.get("https://www.amazon.com/alm/storefront?almBrandId=QW1hem9uIEZyZXNo")
    amazon_driver.maximize_window()

    # select address option
    wait = WebDriverWait(amazon_driver, driver_wait)
    select_address = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/header/div/div[1]/div[1]/div[2]/span/a")))
    select_address.click()

    # enter zipcode - specifically 22201
    enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, "//div/div[2]/div[3]/div[2]/div[1]/div[1]/input")))
    enter_zip.send_keys(str(zipcodes[1]), '\n')

    # close window
    done = wait.until(EC.element_to_be_clickable((By.XPATH, "//span/span/span/button")))
    done.click()

    # search across the different cultural baskets
    all_af_foods = []
    for idx1, foods in enumerate(baskets):
        # search within each basket
        time.sleep(sleep_time)
        af_basket = []
        for idx2, food in enumerate(foods):
            # search for food items
            search = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/header/div/div[1]/div[2]/div/form/div[2]/div[1]/input")))
            
            # clear search bar
            if idx1 > 0 or idx2 > 0:
                search.clear()

            # search for specific food
            search.send_keys(food, "\n")

            # while element not accessible, re-run search
            while not check_exists_by_xpath(amazon_driver, "//div/span/div/div/div[2]"):
                time.sleep(sleep_time)
                search = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/header/div/div[1]/div[2]/div/form/div[2]/div[1]/input")))
                search.clear()
                search.send_keys(food, "\n")

            # get information on top item
            info = wait.until(EC.element_to_be_clickable((By.XPATH, "//div/span/div/div/div[2]")))

            # if shop deli pops up in information, use different path to top item
            if info.text[:9] == "Shop deli":
                info = wait.until(EC.element_to_be_clickable((By.XPATH, "//span[3]/div[2]/div[3]/div/span/div/div/div[2]")))

            # append information to list and then list to list of lists
            af_basket.append(info.text)
        all_af_foods.append(af_basket)

    # close driver
    amazon_driver.quit()

    ## item
    af_item = [[all_af_foods[j][i].split("\n")[0] for i in range(len(all_af_foods[j]))] for j in range(len(all_af_foods))]

    ## price
    af_price = [[all_af_foods[j][i].split("$")[1].split(" ")[0].replace("\n", ".") for i in range(len(all_af_foods[j]))] for j in range(len(all_af_foods))]

    ## af_other_info
    af_other_info = [[re.split("\)", re.split("\(", all_af_foods[j][i])[-1])[0] for i in range(len(all_af_foods[j]))] for j in range(len(all_af_foods))]
    
    # create dataframes
    for i in range(len(af_item)):
        a = np.array(af_item[i])
        b = np.array(af_price[i])
        c = np.array(af_other_info[i])
        d = np.array([zipcodes[1] for i in range(len(c))])
        e = np.array(["Amazon Fresh" for i in range(len(c))])

        # put dataframe columns together and return dataframe
        df = pd.DataFrame({"Item": a, "Price": b, "Other_Info": c, "Location": d, "Store": e})
        df.to_csv(names[i])
        
def sams_club(sam_driver, zipcodes, names, driver_wait = 20, sleep_time = 2, *args):
    '''
    Description:
        Sam's club driver - must already have driver open. Does not currently use zipcodes paramater
    
    Inputs:
        sam_driver:
        zipcodes: list of Arlington zipcodes (for now we only use 22201)
        names: list of names for csv files generated by function - should be the same length as arguments passed for *args
        driver_wait: integer, wait time for driver - default = 20
        sleep_time: integer, system sleep time between certain processes - default = 2
        *args: various staple food baskets to be searched for
    
    Outputs:
        None
    '''
    # creates length variable for the number of backspaces needed
    length = 0

    # loop over baskets
    all_sams_baskets = []
    for basket in args:
        sams_basket = []

        # loop over food items within a basket
        for food in basket:
            wait = WebDriverWait(sam_driver, 20)

            # search for specific food
            search = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[1]/div/div/div[2]/div/nav[2]/div/div[1]/div/div/form/input')))
            search.send_keys("".join(np.array(["\b" for i in range(length)])))
            search.send_keys(food, '\n')
            length = len(food)

            # get information on top item
            info = wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[2]/div[2]/div/div[2]/div[2]/div[2]/div/ul/li[1]')))

            # append information to list and then list to list of lists
            sams_basket.append(info.text)
        all_sams_baskets.append(sams_basket)

    # close driver
    sam_driver.quit()

    ## price
    sams_price = [[float(all_sams_baskets[j][i].split("$")[1].split("\n")[0].split("/")[0]) for i in range(len(all_sams_baskets[j]))] for j in range(len(all_sams_baskets))]

    ## item
    sams_item = [[all_sams_baskets[j][i].split("Top Rated\n")[-1].split("Sponsored \n")[-1].split("Sam'S Exclusive\n")[-1].split("\n")[0] for i in range(len(all_sams_baskets[j]))] for j in range(len(all_sams_baskets))]

    ## other info
    sams_other_info = [[all_sams_baskets[j][i].split("$")[-1].split("\nShipping")[0].split("\nPickup")[0].split("\nFinal")[0].replace("\n", "") for i in range(len(all_sams_baskets[j]))] for j in range(len(all_sams_baskets))]

    # create dataframes 
    for i in range(len(sams_item)):
        a = np.array(sams_item[i])
        b = np.array(sams_price[i])
        c = np.array(sams_other_info[i])
        d = np.array([zipcodes[1] for i in range(len(c))])
        e = np.array(["Sam's Club" for i in range(len(c))])

        # put dataframe columns together and return dataframe
        df = pd.DataFrame({"Item": a, "Price": b, "Other_Info": c, "Location": d, "Store": e})
        df.to_csv(names[i])

### LIDL
def get_lidl_zipcodes(zipcodes, driver_wait = 20, sleep_time = 2):
    '''
    Description:
        Obtain unique Lidl locations in Arlington area
    
    Inputs:
        zipcodes: list of Arlington zipcodes
        driver_wait: integer, wait time for driver - default = 20
        sleep_time: integer, system sleep time between certain processes, default = 2
    
    Outputs:
        stores: 
        short_lidl_zip: list of Arlington zipcodes with unique Lidls
    '''
    # create Lidl driver and maximize window
    lidl_driver = webdriver.Chrome(ChromeDriverManager().install())
    lidl_driver.get("https://www.lidl.com/")
    lidl_driver.maximize_window()    

    # sign in
    wait = WebDriverWait(lidl_driver, driver_wait)
    sign_in = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/header/div/nav[2]/ul/li[3]/a")))
    sign_in.click()

    # enter email and password, and click sign in bottom
    email = wait.until(EC.element_to_be_clickable((By.ID, "input0")))
    email.send_keys("zacharyalerte@gmail.com")
    password = wait.until(EC.element_to_be_clickable((By.ID, "input1")))
    password.send_keys("uvaSDAD2021!")
    sign_in = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div/div/div[1]/form/div/button[5]")))
    sign_in.click()

    # select my store to enter zipcodes
    my_store = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/header/div/nav[2]/ul/li[2]/a/span")))
    my_store.click()

    # loop over zipcodes
    lidl_zips = []
    lidl_store = []
    for zipcode in zipcodes:
        # enter zipcode
        send_zip = wait.until(EC.element_to_be_clickable((By.ID, "input2")))
        send_zip.send_keys("\b\b\b\b\b")
        send_zip.send_keys(str(zipcode), '\n')

        # check if top store for zipcode is different
        time.sleep(sleep_time)
        if check_exists_by_xpath(lidl_driver, "/html/body/div[2]/div/div[2]/div[2]/div/div/div/div[1]/div[3]/div/div/button"):
            # set top store as my store
            set_as_my_store = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/div[2]/div/div/div/div[1]/div[3]/div/div/button")))
            set_as_my_store.click()
            
            # skip location if it is an express store
            express = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/div[2]/div/div/div/div[1]/div[2]/div/button/div/h4")))
            if "EXPRESS STORE" in express.text:
                continue
            # if not an express store
            else:
                #
                lidl_zips.append(zipcode)

                #
                element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "address")))
                lidl_store.append(element.text)

    # obtain array of uniuqe stores and indexes for zipcodes
    stores, lidl_idxs = np.unique(np.array(lidl_store), return_index = True)
    short_lidl_zips = np.array(lidl_zips)[lidl_idxs]

    # close driver
    lidl_driver.quit()
    return stores, short_lidl_zips

def lidl(names, zipcodes, short_lidl_zips = [], sleep_time = 2, driver_wait = 20, get_zips = True, *args):
    '''
    Description:
        Run all code for Lidl and get dataframes defined by name
    
    Inputs:
        names: list of names for csv files generated by function - should be the same length as arguments passed for *args
        zipcodes: list of Arlington zipcodes
        short_lidl_zips: shortened list of zipcodes passed into function, default = []
        driver_wait: integer, wait time for driver - default = 20
        sleep_time: integer, system sleep time between certain processes, default = 2
        get_zips: boolean, True if you want to run function to get unique Lidl zipcodes, False if you have unique Lidl Zipcodes passed in as short_lidl_zips
        *args: various staple food baskets to be searched for
    
    Outputs:
        None
    '''
    # get zipcodes if get_zips passed into function
    if get_zips:
        stores, short_lidl_zips = get_lidl_zipcodes(zipcodes, driver_wait = 20)
    
    # create Lidl driver and maximize window
    lidl_driver = webdriver.Chrome(ChromeDriverManager().install())
    lidl_driver.get("https://www.lidl.com/")
    lidl_driver.maximize_window()

    # sign in
    #
    wait = WebDriverWait(lidl_driver, 20)
    sign_in = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/header/div/nav[2]/ul/li[3]/a")))
    sign_in.click()

    # enter email and password
    email = wait.until(EC.element_to_be_clickable((By.ID, "input0")))
    email.send_keys("zacharyalerte@gmail.com")
    password = wait.until(EC.element_to_be_clickable((By.ID, "input1")))
    password.send_keys("uvaSDAD2021!")

    #
    sign_in = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div/div/div[1]/form/div/button[5]")))
    sign_in.click()

    #
    all_lidl_info = []
    for lidl_zip in short_lidl_zips:
        #
        my_store = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/header/div/nav[2]/ul/li[2]/a/span")))
        my_store.click()

        #
        send_zip = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/div[2]/div/div/div/div[1]/form/div[2]/div/div/div/div/input")))
        send_zip.send_keys("\b\b\b\b\b")
        send_zip.send_keys(str(lidl_zip), '\n')

        #
        if  check_exists_by_class_name(lidl_driver, "clickable button clickable--theme-primary clickable--size-base clickable--color-green clickable--bold-weight clickable--regular-font"):
            set_as_my_store = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "clickable button clickable--theme-primary clickable--size-base clickable--color-green clickable--bold-weight clickable--regular-font")))
            set_as_my_store.click()
            time.sleep(sleep_time)

        # empty list to store information on a store basket
        store_lidl_info = []
        
        # loop over all cultural baskets
        for basket in args:
            # 
            lidl_info = []
            for i in range(len(basket)):
                #
                search = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/header/div/nav[2]/ul/li[1]/button/span/span")))
                search.click()

                #
                search_2 = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/header/div/div[1]/form/div/div/div/div/div/input")))
                search_2.send_keys(basket[i], "\n")

                # get food info and append to list
                element = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[2]/div/div[2]/div[2]/div/div/div[3]/div/ul/li[1]/div/div/a/div[2]")))
                lidl_info.append(element.text)

            # append list to lists
            store_lidl_info.append(lidl_info)

        # append list of lists to outer list
        all_lidl_info.append(store_lidl_info)

    # close driver
    lidl_driver.quit()
    
    # 
    for j in range(len(all_lidl_info[0])):
        ## price
        lidl_price = [[float(all_lidl_info[k][j][i].split("*\n")[0].replace("$", "")) for i in range(len(all_lidl_info[k][j]))] for k in range(len(all_lidl_info))]
        lidl_price = np.array([item for sublist in lidl_price for item in sublist])

        ## item
        lidl_item = [[all_lidl_info[k][j][i].split("*\n")[1].split("\n")[0] if len(re.findall("\n", all_lidl_info[k][j][i])) != 3 else all_lidl_info[k][j][i].split("*\n")[1].split("\n")[1] for i in range(len(all_lidl_info[k][j]))] for k in range(len(all_lidl_info))]
        lidl_item = np.array([item for sublist in lidl_item for item in sublist])

        ## other info
        lidl_other_info = [[all_lidl_info[k][j][i].split("*\n")[1].split("\n")[-1] for i in range(len(all_lidl_info[k][j]))] for k in range(len(all_lidl_info))]
        lidl_other_info = np.array([item for sublist in lidl_other_info for item in sublist])

        ## zipcode and Lidl
        d = np.array([stores[i].replace("\n", ", ") for i in range(len(stores)) for _ in range(len(all_lidl_info[0][j]))])
        e = np.array(["Lidl" for i in range(len(d))])

        # condense information into dataframe
        df = pd.DataFrame({"Item": lidl_item, "Price": lidl_price, "Other_Info": lidl_other_info, "Location": d, "Store": e})
        df.to_csv(names[j])

### Food Lion
def food_lion(baskets, names, zipcodes, driver_wait = 20, sleep_time = 2):
    '''
    Description:
        Generates dataframes for each cultural basket at 20330 zipcode in Arlington (may need to test other zipcodes in the county to ensure that the pricing is consistent across the county)
    
    Inputs:
        baskets:
        names:
        zipcodes:
        driver_wait: integer, wait time for driver - default = 20
        sleep_time: integer, system sleep time between certain processes, default = 2
    
    Outputs:
        None
    '''
    #
    fl_driver = webdriver.Chrome(ChromeDriverManager().install())
    while True:
        try:
            #
            fl_driver.get("https://foodlion.com/")
            fl_driver.maximize_window()

            #
            wait = WebDriverWait(fl_driver, driver_wait)
            shop_online = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[7]/div[3]/div[1]/div[2]/header[2]/div/div[2]/div/div/ul/li[1]/a")))
            shop_online.click()

            #
            shop_now = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[7]/div[3]/div[1]/div[2]/header[2]/div/div[2]/div/div/ul/li[1]/div/div/ul/li[1]/a")))
            shop_now.click()

            #
            delivery = wait.until(EC.element_to_be_clickable((By.XPATH, "//span/shopping-context-item[2]/button")))
            delivery.click()

            #
            enter_zip = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div[1]/div/div/div/div/div[2]/div/div/div[2]/div/div/form/div[1]/div/input")))
            enter_zip.send_keys(str(zipcodes[0]), '\n')

            #
            fl_all_info = []
            for basket in baskets:
                #
                fl_food_info = []
                for food in basket:
                    #
                    search = wait.until(EC.element_to_be_clickable((By.ID, "search-nav-input")))
                    search.clear()
                    search.send_keys(food, "\n")

                    # extract information on top product
                    time.sleep(sleep_time)
                    info = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "cell-content-wrapper")))
                    fl_food_info.append(info.text)

                #
                fl_food_info = np.array(fl_food_info)
                fl_all_info.append(fl_food_info)

            # item
            fl_item = [[fl_all_info[j][i].split("\n")[0] for i in range(len(fl_all_info[j]))] for j in range(len(fl_all_info))]

            # price
            fl_price = [[float(re.split("Price\n\$|Original price:\n\$", fl_all_info[j][i])[-1].split(" ")[0]) for i in range(len(fl_all_info[j]))] for j in range(len(fl_all_info))]

            # other info
            fl_other_info = [[re.findall("\(.*\)", fl_all_info[j][i])[-1] if len(re.findall("\(.*\)", fl_all_info[j][i])) >= 1 else "" for i in range(len(fl_all_info[j]))] for j in range(len(fl_all_info))]

            # close driver
            fl_driver.quit()
            break

        #
        except ElementClickInterceptedException:
            if check_exists_by_class_name(fl_driver, "Sclose"):
                close_popup = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "Sclose")))
                close_popup.click()

        # create dataframes
        for i in range(len(fl_item)):
            a = np.array(fl_item[i])
            b = np.array(fl_price[i])
            c = np.array(fl_other_info[i])
            d = np.array([zipcodes[0] for i in range(len(c))])
            e = np.array(["Food Lion" for i in range(len(c))])

            # put dataframe columns together and return dataframe
            df = pd.DataFrame({"Item": a, "Price": b, "Other_Info": c, "Location": d, "Store": e})
            df.to_csv(names[i])