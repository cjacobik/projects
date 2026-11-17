# ESP32 CYD Aircraft Radar

A touchscreen aircraft radar for the **ESP32 Cheap Yellow Display (CYD), dual-USB variant**, built with Arduino and PlatformIO. It plots nearby aircraft on a schematic map, shows a short flight list, and displays live details for the nearest aircraft.

Aircraft positions come from [adsb.fi](https://adsb.fi/). Optional airline, route, and aircraft-model details come from [AirLabs](https://airlabs.co/). **No ADSBDB API is used.**

> **Project status:** Hobby project tested and iterated on a dual-USB CYD. Hardware revisions and API behavior vary. The map is schematic and **not for navigation**.

## Features

- **Three screens, in order:** **NEAR → RADAR → LIST → NEAR**. The device starts on NEAR.
- **NEAR:** Automatically shows the closest aircraft from the latest successful position update, including flight number, airline, FROM/TO, distance (miles), altitude (feet), speed (mph), eight-point compass heading, and manufacturer/model when available.
- **RADAR:** Touch an aircraft to select and highlight it; use **NEXT** to cycle aircraft. Selected-aircraft information includes compass heading.
- **LIST:** A four-column table of the **five nearest visible aircraft**: flight number, FROM, TO, and distance. Tap a row to return to RADAR with that aircraft highlighted.
- **Schematic regional roads:** I-64, I-81, US-29, US-250, US-33, US-11, US-340, VA-20, and VA-151, plus selected cities and airports near Charlottesville, Virginia. Roads are approximate and are embedded in firmware; no map-tile requests.
- **30-second aircraft refresh** from adsb.fi, with connection, HTTP, JSON, and memory diagnostics on the serial monitor.
- **5–160 mile display zoom**. The default **aircraft fetch radius is 40 miles** to reduce ESP32 memory pressure; zooming out does *not* automatically increase the API fetch radius.
- **AirLabs usage controls:** One lookup attempt per callsign + ICAO hex combination **per boot**, including failed attempts. RADAR selections have lookup priority; LIST enriches up to five flights; NEAR looks up its current closest flight. Results are cached in RAM.

## Hardware

- ESP32 **dual-USB CYD** with a **320 × 240 ST7789** display
- XPT2046 resistive touchscreen
- Wi-Fi connection
- USB cable for programming and power

The TFT and touch controller use separate SPI buses. The firmware includes touch calibration values for the tested board; other boards may need recalibration.

## Build with PlatformIO

1. Install [Visual Studio Code](https://code.visualstudio.com/) and the [PlatformIO IDE extension](https://platformio.org/install/ide?install=vscode).
2. Create a PlatformIO project using the **Espressif ESP32 Dev Module** board and the **Arduino** framework.
3. Put the firmware in `src/main.cpp`.
4. Use a `platformio.ini` configuration like this:

```ini
[env:cyd2usb]
platform = espressif32
board = esp32dev
framework = arduino
monitor_speed = 115200
upload_speed = 115200

lib_deps =
    bodmer/TFT_eSPI
    bblanchon/ArduinoJson@^7.0.0
    paulstoffregen/XPT2046_Touchscreen

build_flags =
    -D USER_SETUP_LOADED=1
    -D ST7789_DRIVER=1
    -D TFT_WIDTH=240
    -D TFT_HEIGHT=320
    -D TFT_MISO=12
    -D TFT_MOSI=13
    -D TFT_SCLK=14
    -D TFT_CS=15
    -D TFT_DC=2
    -D TFT_RST=-1
    -D TFT_BL=21
    -D TFT_BACKLIGHT_ON=HIGH
    -D USE_HSPI_PORT=1
    -D SPI_FREQUENCY=27000000
    -D TFT_RGB_ORDER=TFT_BGR
    -D LOAD_GLCD=1
    -D LOAD_FONT2=1
```

5. Edit the configuration block near the top of `src/main.cpp`:

```cpp
const char* WIFI_SSID = "YOUR_WIFI_NAME";
const char* WIFI_PASS = "YOUR_WIFI_PASSWORD";
const char* AIRLABS_API_KEY = "YOUR_AIRLABS_API_KEY";
constexpr double HOME_LAT = 38.03;   // Example only
constexpr double HOME_LON = -78.48;  // Example only
```

6. Build and upload with PlatformIO. Open the serial monitor at **115200 baud** to view diagnostics.

### Keep credentials out of GitHub

**Do not commit real Wi-Fi passwords or API keys.** The current firmware has inline configuration values, so replace real values with placeholders **before pushing `src/main.cpp`**. For ongoing development, move secrets to a locally ignored header (and keep a tracked example file). If a key was accidentally committed, revoke/rotate it; deleting it in a later commit does not remove it from Git history.

## Controls

| Screen | Behavior |
|---|---|
| **NEAR** | Closest aircraft is chosen automatically after successful adsb.fi refreshes; NEXT is hidden |
| **RADAR** | Tap an aircraft to highlight it and prioritize an AirLabs lookup; NEXT cycles through visible aircraft |
| **LIST** | Shows the five nearest visible aircraft; tap a row to open RADAR with that aircraft selected; NEXT remains available |
| **Screen switch** | Cycles **NEAR → RADAR → LIST → NEAR** |
| **− MI / + MI** | Zooms the displayed radar range out/in |
| **REFRESH** | Requests an aircraft update, subject to the minimum request gap |

## APIs and request limits

### adsb.fi — aircraft positions

The firmware requests aircraft near the configured latitude/longitude using the adsb.fi open-data endpoint. Its default refresh interval is **30 seconds**. A failed request is retried after a longer delay. The firmware logs HTTP status, response size, heap usage, and JSON parsing errors.

### AirLabs — flight details

AirLabs' single-flight endpoint is used to retrieve available fields such as:

- `airline_name`, `dep_iata`, `arr_iata`
- `manufacturer`, `model`, `status`
- `flight_icao`, `hex` (used for identity checks when available)

To conserve free-tier quota, the firmware records each attempted **callsign + aircraft hex** lookup in an in-memory ledger. **A failed lookup is not retried during the same boot.** RADAR selections take priority; LIST only considers its five displayed aircraft; NEAR requests details for its nearest aircraft. **The ledger resets on reboot**, so a restart can result in repeat requests. The cache has a finite capacity.

**Important:** Callsigns can be reused, flight routes can change, and third-party data may be missing or inaccurate. A route is not guaranteed to represent the currently operating flight. The display distinguishes verified hex matches from unverified route data where available.

Review the current [AirLabs plans and API terms](https://airlabs.co/) before using the service; free-tier access and quotas may change.

## Troubleshooting

| Symptom | What to check |
|---|---|
| Stuck on CONNECTING | Wi-Fi SSID/password, 2.4 GHz network availability, and serial output |
| HTTP error / LOADING | Network connectivity, endpoint availability, request timeouts, and serial diagnostics |
| `JSON ERROR: IncompleteInput` | Incomplete/truncated response or memory pressure; try a smaller `FETCH_RADIUS_MI` |
| No aircraft visible | Home coordinates, current aircraft activity, display zoom, and fetch radius |
| FROM/TO shows `N/A` | Callsign missing, AirLabs returned no usable route, lookup failed, or this flight was already attempted |
| No AirLabs request after a tap | Check serial logs for cache hits, previous failed attempts, or missing callsign; reboot resets the attempt ledger |
| Touch locations are wrong | Recalibrate `RAW_X_MIN`, `RAW_X_MAX`, `RAW_Y_MIN`, `RAW_Y_MAX` for your hardware |
| Display colors/orientation are wrong | Verify ST7789 driver, BGR setting, display rotation, and your exact CYD board variant |

## Map limitations

The bundled roads and place labels are **approximate geographic sketches centered on the Charlottesville region**, not OpenStreetMap geometry. If you change `HOME_LAT` and `HOME_LON` to another region, replace the hardcoded road/city/airport coordinates as well.

## Project layout

```text
.
├── platformio.ini
├── README.md
└── src/
    └── main.cpp
```

## Disclaimer

For personal, educational, and hobby use. Not suitable for aviation safety, navigation, or operational decision-making. Aircraft position and flight information depend on third-party services and may be delayed, incomplete, or incorrect.
