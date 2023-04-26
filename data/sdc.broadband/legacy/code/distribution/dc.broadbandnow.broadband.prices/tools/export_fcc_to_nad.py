import os
import logging
from combine_csv import combine_csv
import pandas as pd
from tqdm import tqdm

# Given a directory of fcc geocoded csvs, extract files to the national address database repository


def clean_geocoded_csv(df, state, county):
    expected_columns = ["state", "county", "longitude", "latitude", "address"]
    cdf = pd.DataFrame(columns=expected_columns)

    assert "street" in df.columns, "street missing in (%s,%s)" % (state, county)
    assert "coordinate" in df.columns, "coordinate missing in (%s,%s)" % (state, county)

    df = df[["street", "coordinate"]].dropna()  # get only matches and remove empty rows
    cdf["address"] = df["street"].apply(lambda x: x.lower())
    cdf["longitude"] = df["coordinate"].apply(lambda x: x.split(",")[0])
    cdf["latitude"] = df["coordinate"].apply(lambda x: x.split(",")[1])
    cdf["state"] = state
    cdf["county"] = county

    return cdf


def get_state_county_prefix(county_fip):
    fips_county = pd.read_csv(
        "https://raw.githubusercontent.com/uva-bi-sdad/national_address_database/main/data/fips_county.csv",
        dtype={"fips": object},
    )
    fips_state = pd.read_csv(
        "https://raw.githubusercontent.com/uva-bi-sdad/national_address_database/main/data/fips_state.csv",
        dtype={"fips": object},
    )

    logging.info("Searching for state county prefix of: %s" % county_fip)

    state_abbr = fips_state[fips_state["fips"] == county_fip[:2]]["abbr"].values[0]
    county_name = fips_county[fips_county["fips"] == county_fip]["county"].values[0]

    return state_abbr, county_name


def main():
    export_dir = "temp_nad/"
    for f in tqdm(os.listdir(".")):
        if not os.path.isdir(f):
            continue
        # print("[%s]: %s" % (f[:7] == "temp_01", f[:7]))
        if f[:7] != "temp_01":
            continue
        if f.split("_")[-1] != "fcc":  # only parse the fcc directories
            continue

        print(f)
        # print(f[:7])
        county_fip = f.split("_")[1]
        export_filepath = "%s_geocoded.csv.xz" % county_fip
        logging.info(county_fip)
        df = combine_csv(f)
        state, county = get_state_county_prefix(county_fip)
        prefix = "%s_%s" % (state, county)
        cdf = clean_geocoded_csv(df, state, county)
        export_filepath = os.path.join(export_dir, "%s.csv.xz" % prefix)
        cdf.to_csv(
            export_filepath,
            index=False,
        )


if __name__ == "__main__":
    log_level = logging.INFO
    logging.basicConfig(format="%(levelname)s: %(message)s", level=log_level)
    main()
