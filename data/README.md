# Data

## Input file (required, not committed)

Place the source workbook here:

```
data/raw/Qatar_Data_V.1.0.xlsx
```

The path and sheet name are configured in [`config/config.yml`](../config/config.yml).
The file is **not** version-controlled (it lives outside git on purpose); the
pipeline reads it via `here::here(config$data$path)` so no absolute machine
paths appear anywhere in the code.

## Expected schema — sheet `data_for_model`

A country-year panel. Columns consumed by the analysis:

| Column      | Type      | Description                                            |
|-------------|-----------|--------------------------------------------------------|
| `country`   | character | Cross-sectional unit (panel index 1).                  |
| `year`      | integer   | Time index (panel index 2).                            |
| `ln_trade`  | numeric   | Log of bilateral trade — **dependent variable**.       |
| `ln_cti`    | numeric   | Log of competitiveness/trade index.                    |
| `ln_gdpc`   | numeric   | Log of GDP per capita.                                 |
| `distance`  | numeric   | Geographic distance (time-invariant; absorbed by FE).  |
| `crises`    | numeric   | Dummy = 1 in blockade/crisis years, else 0.            |

Derived in-pipeline (not expected in the file):

| Column       | Description                                              |
|--------------|----------------------------------------------------------|
| `d_ln_gdpc`  | First difference of `ln_gdpc` per country (`x_t - x_{t-1}`); leading NA per country. |

## Groups

- **Group A** (blockading states): Saudi, UAE, Bahrain, Egypt.
- **Group B**: all other countries (complement of Group A).
