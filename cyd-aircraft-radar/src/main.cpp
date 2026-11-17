#include <Arduino.h>
#include <WiFi.h>
#include <WiFiClientSecure.h>
#include <HTTPClient.h>
#include <ArduinoJson.h>
#include <TFT_eSPI.h>
#include <XPT2046_Touchscreen.h>
#include <SPI.h>
#include <math.h>

// ================= USER CONFIGURATION =================
const char* WIFI_SSID = "YOUR_WIFI_NAME";
const char* WIFI_PASS = "YOUR_WIFI_PASSWORD";
const char* AIRLABS_API_KEY = "YOUR_AIRLABS_API_KEY";
constexpr double HOME_LAT = 38.03;   // Example: Charlottesville, VA
constexpr double HOME_LON = -78.48;
constexpr uint32_t REFRESH_MS = 30000; // 30 seconds
constexpr float MIN_RANGE_MI = 5.0f;
constexpr float MAX_RANGE_MI = 160.0f;
constexpr float DEFAULT_RANGE_MI = 40.0f;
constexpr float FETCH_RADIUS_MI = 40.0f; // Start small; increase after verifying memory
constexpr uint32_t ERROR_RETRY_MS = 60000;
constexpr uint32_t MIN_REQUEST_GAP_MS = 5000;
constexpr bool SHOW_VECTOR_MAP = true;
constexpr bool ENABLE_ROUTE_LOOKUP = true; // AirLabs: one attempt per callsign+hex per boot

// Map coordinates below are approximate landmarks and schematic road corridors.
// Edit/replace them for another HOME_LAT / HOME_LON. NOT FOR NAVIGATION.
struct GeoPoint { float lat, lon; };
struct MapPlace { float lat, lon; const char* label; };
const MapPlace CITIES[] = {
  {38.0293f, -78.4767f, "CVILLE"},
  {38.4496f, -78.8689f, "HARRISONBURG"},
  {38.1496f, -79.0717f, "STAUNTON"},
  {37.4138f, -79.1422f, "LYNCHBURG"},
  {37.5407f, -77.4360f, "RICHMOND"}
};
const MapPlace AIRPORTS[] = {
  {38.1386f, -78.4529f, "CHO"},
  {38.2638f, -78.8964f, "SHD"},
  {37.3267f, -79.2004f, "LYH"},
  {37.5052f, -77.3197f, "RIC"}
};
// Simplified geographic corridors; approximate only, NOT for navigation.
// Road points are in travel order. Lines are clipped to the circular radar.
// Interstates
const GeoPoint I64[] = {
  {38.153f,-79.071f},{38.092f,-78.868f},{38.070f,-78.724f},
  {38.052f,-78.608f},{38.031f,-78.510f},{38.024f,-78.420f},
  {37.993f,-78.315f},{37.960f,-78.205f},{37.875f,-78.105f},
  {37.762f,-77.874f},{37.650f,-77.666f},{37.550f,-77.445f}
};
const GeoPoint I81[] = {
  {38.680f,-78.668f},{38.525f,-78.775f},{38.449f,-78.869f},
  {38.323f,-78.913f},{38.264f,-78.941f},{38.150f,-79.072f},
  {38.082f,-79.164f},{37.968f,-79.226f},{37.800f,-79.340f}
};
// US highways
const GeoPoint US29[] = {
  {38.410f,-78.370f},{38.285f,-78.423f},{38.185f,-78.438f},
  {38.138f,-78.451f},{38.092f,-78.473f},{38.060f,-78.490f},
  {38.029f,-78.478f},{37.989f,-78.496f},{37.913f,-78.565f},
  {37.817f,-78.661f},{37.693f,-78.797f},{37.566f,-78.915f},
  {37.414f,-79.142f}
};
const GeoPoint US250[] = {
  {38.165f,-79.073f},{38.115f,-78.895f},{38.083f,-78.755f},
  {38.064f,-78.620f},{38.046f,-78.528f},{38.038f,-78.501f},
  {38.030f,-78.477f},{38.025f,-78.432f},{38.027f,-78.368f},
  {38.020f,-78.295f},{37.985f,-78.180f},{37.915f,-78.040f},
  {37.804f,-77.790f},{37.625f,-77.540f},{37.542f,-77.437f}
};
const GeoPoint US33[] = {
  {38.450f,-78.870f},{38.443f,-78.740f},{38.421f,-78.615f},
  {38.377f,-78.485f},{38.307f,-78.355f},{38.250f,-78.205f},
  {38.170f,-78.030f},{38.100f,-77.870f}
};
const GeoPoint US11[] = {
  {38.660f,-78.705f},{38.450f,-78.870f},{38.264f,-78.940f},
  {38.150f,-79.071f},{38.067f,-79.178f},{37.950f,-79.260f},
  {37.780f,-79.375f}
};
const GeoPoint US340[] = {
  {38.620f,-78.610f},{38.450f,-78.640f},{38.340f,-78.665f},
  {38.220f,-78.800f},{38.100f,-78.860f},{38.010f,-78.870f},
  {37.890f,-78.850f}
};
// State routes: shown at closer zoom levels to avoid clutter.
const GeoPoint VA20[] = {
  {38.102f,-78.475f},{38.067f,-78.471f},{38.030f,-78.477f},
  {37.990f,-78.451f},{37.925f,-78.416f},{37.850f,-78.348f},
  {37.765f,-78.300f},{37.665f,-78.268f}
};
const GeoPoint VA151[] = {
  {38.015f,-78.760f},{37.970f,-78.780f},{37.915f,-78.820f},
  {37.845f,-78.875f},{37.790f,-78.915f}
};
// ================= DISPLAY AND TOUCH =================
TFT_eSPI tft;
constexpr int TOUCH_CS = 33, TOUCH_IRQ = 36;
XPT2046_Touchscreen touch(TOUCH_CS, TOUCH_IRQ);
constexpr int RAW_X_MIN=671, RAW_X_MAX=3462;
constexpr int RAW_Y_MIN=490, RAW_Y_MAX=3657;
constexpr int CX=101, CY=115, RADAR_R=82;
constexpr uint16_t BG=0x0841, GRID=0x2365, GREEN=0x07E0;
constexpr uint16_t YELLOW=0xFFE0, WHITE=0xFFFF, GRAY=0x8410;
constexpr uint16_t ROAD_INTERSTATE=0x4A69; // muted blue
constexpr uint16_t ROAD_US=0x73AE;         // blue-gray
constexpr uint16_t ROAD_STATE=0x39C7;      // subdued gray
constexpr uint16_t CITY=0xAD55, AIRPORT=0x07FF;

// ================= AIRCRAFT =================
constexpr int MAX_PLANES=80;
struct Aircraft {
  String id, callsign;
  float distanceMi, eastMi, northMi, heading, speedKt;
  int altitudeFt;
  bool hasHeading, hasSpeed, hasAltitude;
};
Aircraft planes[MAX_PLANES];
Aircraft incoming[MAX_PLANES]; // global: avoid large stack allocation
int planeCount=0, selectedPlane=-1;
float radarRangeMi=DEFAULT_RANGE_MI;
enum ScreenView { DETAILS_VIEW, RADAR_VIEW, LIST_VIEW };
ScreenView screenView=DETAILS_VIEW;
int listPage=0;
bool dataValid=false, fetchRequested=true;
bool hasAttemptedFetch=false;
uint32_t lastFetch=0, nextFetch=0;
String statusText="STARTING";
// Fixed attempt ledger: NEVER evict entries, because eviction could repeat a paid lookup.
// Once full, further AirLabs calls are disabled until reboot. Failed attempts are retained.
constexpr int ROUTE_CACHE_SIZE=128;
struct RouteInfo {
  String callsign, hex, airline, origin, destination, manufacturer, model, flightStatus;
  bool hasRoute=false, attempted=false, verifiedHex=false;
};
RouteInfo routeCache[ROUTE_CACHE_SIZE];
int routeCacheCount=0;
String routePending=""; // callsign; resolved against current aircraft at request time
String routeBusy="";
String radarPriorityCall="", radarPriorityHex=""; // tap always wins over list/near
uint32_t lastRouteLookup=0;

String trimmedCallsign(const String &s) {
  String c=s;c.trim();c.toUpperCase();
  // ADS-B flight fields are padded with spaces; strip them consistently.
  c.replace(" ","");
  return c;
}
// Airline inference from ICAO operating callsign, NOT a route lookup.
String airlineFromCallsign(const String &c) {
  if(c.startsWith("DAL"))return "Delta Air Lines";
  if(c.startsWith("AAL"))return "American Airlines";
  if(c.startsWith("UAL"))return "United Airlines";
  if(c.startsWith("SWA"))return "Southwest Airlines";
  if(c.startsWith("JBU"))return "JetBlue";
  if(c.startsWith("ASA"))return "Alaska Airlines";
  if(c.startsWith("FFT"))return "Frontier Airlines";
  if(c.startsWith("NKS"))return "Spirit Airlines";
  if(c.startsWith("RPA"))return "Republic Airways";
  if(c.startsWith("SKW"))return "SkyWest Airlines";
  if(c.startsWith("ENY"))return "Envoy Air";
  if(c.startsWith("PDT"))return "Piedmont Airlines";
  if(c.startsWith("EDV"))return "Endeavor Air";
  if(c.startsWith("JIA"))return "PSA Airlines";
  return "AIRLINE N/A";
}
String normalizedHex(const String &s) {
  String h=s;h.trim();h.toUpperCase();return h;
}
const RouteInfo* findRoute(const String &callsign, const String &hex) {
  for(int i=0;i<routeCacheCount;i++)
    if(routeCache[i].callsign==callsign && routeCache[i].hex==normalizedHex(hex))
      return &routeCache[i];
  return nullptr;
}
String shorten(const String &s, int maxChars) {
  if((int)s.length()<=maxChars)return s;
  return s.substring(0,maxChars-1)+"~";
}
bool visible(const Aircraft &a);
int effectiveSelectedPlane() {
  for(int i=0;i<planeCount;i++)if(visible(planes[i]))return i;
  return -1;
}
bool validCallsign(const Aircraft &a) {
  String c=trimmedCallsign(a.callsign);
  return !c.isEmpty() && c!="UNKNOWN" && c!=normalizedHex(a.id);
}
void queuePlaneRoute(int idx) {
  if(!ENABLE_ROUTE_LOOKUP || idx<0 || idx>=planeCount)return;
  const Aircraft &a=planes[idx];
  if(!validCallsign(a))return;
  String c=trimmedCallsign(a.callsign);
  if(!findRoute(c,a.id) && routeCacheCount<ROUTE_CACHE_SIZE)routePending=c;
}
void queueSelectedRoute() {
  // Only NEAR auto-fetches; radar selection by NEXT must NOT trigger API calls.
  if(screenView==DETAILS_VIEW)queuePlaneRoute(effectiveSelectedPlane());
}
void storeRoute(const RouteInfo &r) {
  if(routeCacheCount<ROUTE_CACHE_SIZE)routeCache[routeCacheCount++]=r;
}


float radiansOf(float d) { return d*PI/180.0f; }
void offsets(float lat, float lon, float &east, float &north) {
  float a=radiansOf(HOME_LAT), b=radiansOf(lat);
  north=3958.7613f*(b-a);
  east=3958.7613f*cosf((a+b)/2.0f)*radiansOf(lon-HOME_LON);
}
void locateAircraft(float lat, float lon, Aircraft &a) {
  offsets(lat,lon,a.eastMi,a.northMi);
  // Local tangent-plane distance is sufficiently accurate at this scale.
  a.distanceMi=sqrtf(a.eastMi*a.eastMi+a.northMi*a.northMi);
}
void geoToPixel(float lat, float lon, int &x, int &y) {
  float east,north; offsets(lat,lon,east,north);
  x=CX+lroundf(east/radarRangeMi*RADAR_R);
  y=CY-lroundf(north/radarRangeMi*RADAR_R);
}
void planeToPixel(const Aircraft &a, int &x, int &y) {
  x=CX+lroundf(a.eastMi/radarRangeMi*RADAR_R);
  y=CY-lroundf(a.northMi/radarRangeMi*RADAR_R);
}
bool inCircle(int x,int y,int padding=0) {
  int dx=x-CX,dy=y-CY,r=RADAR_R-padding;
  return dx*dx+dy*dy<=r*r;
}
bool visible(const Aircraft &a) {return a.distanceMi<=radarRangeMi;}
int visibleCount() {int n=0;for(int i=0;i<planeCount;i++)if(visible(planes[i]))n++;return n;}

void drawRoad(const GeoPoint *pts, size_t n, uint16_t color) {
  for(size_t i=1;i<n;i++) {
    int x1,y1,x2,y2;
    geoToPixel(pts[i-1].lat,pts[i-1].lon,x1,y1);
    geoToPixel(pts[i].lat,pts[i].lon,x2,y2);
    // Clip the line to the radar bounding box, then mask outside circle.
    // TFT_eSPI line drawing is clipped to screen, but not the radar circle.
    const int steps=max(abs(x2-x1),abs(y2-y1));
    if(steps==0)continue;
    // Skip excessively long rasterization work on extreme zooms.
    if(steps>50000)continue;
    for(int s=0;s<=steps;s+=2) {
      int x=x1+(int)((int64_t)(x2-x1)*s/steps);
      int y=y1+(int)((int64_t)(y2-y1)*s/steps);
      if(inCircle(x,y,2))tft.drawPixel(x,y,color);
    }
  }
}
void drawMapBackground() {
  if(!SHOW_VECTOR_MAP)return;
  // Draw roads before city/airport labels and aircraft symbols.
  // At wide zoom, show only the most prominent corridors.
  drawRoad(I64, sizeof(I64)/sizeof(I64[0]), ROAD_INTERSTATE);
  drawRoad(I81, sizeof(I81)/sizeof(I81[0]), ROAD_INTERSTATE);
  drawRoad(US29, sizeof(US29)/sizeof(US29[0]), ROAD_US);
  drawRoad(US250, sizeof(US250)/sizeof(US250[0]), ROAD_US);
  if(radarRangeMi <= 80.0f) {
    drawRoad(US33, sizeof(US33)/sizeof(US33[0]), ROAD_US);
    drawRoad(US11, sizeof(US11)/sizeof(US11[0]), ROAD_US);
    drawRoad(US340, sizeof(US340)/sizeof(US340[0]), ROAD_US);
  }
  if(radarRangeMi <= 40.0f) {
    drawRoad(VA20, sizeof(VA20)/sizeof(VA20[0]), ROAD_STATE);
    drawRoad(VA151, sizeof(VA151)/sizeof(VA151[0]), ROAD_STATE);
  }
  for(const auto &c:CITIES) {
    int x,y;geoToPixel(c.lat,c.lon,x,y);
    if(!inCircle(x,y,12))continue;
    tft.fillCircle(x,y,2,CITY);
    if(radarRangeMi>=20 && x<151 && y>32 && y<184) {
      tft.setTextColor(CITY,BG);
      tft.drawString(c.label,x+3,y-9,1);
    }
  }
  for(const auto &a:AIRPORTS) {
    int x,y;geoToPixel(a.lat,a.lon,x,y);
    if(!inCircle(x,y,9))continue;
    tft.drawRect(x-3,y-3,7,7,AIRPORT);
    tft.setTextColor(AIRPORT,BG);
    if(x<155 && y>32 && y<184)tft.drawString(a.label,x+5,y-5,1);
  }
}
void drawPlaneIcon(int x,int y,float heading,uint16_t color) {
  float ang=radiansOf(heading),dx=sinf(ang),dy=-cosf(ang);
  float wx=-dy,wy=dx;
  tft.fillTriangle(
    x+lroundf(dx*8),y+lroundf(dy*8),
    x-lroundf(dx*3)+lroundf(wx*5),y-lroundf(dy*3)+lroundf(wy*5),
    x-lroundf(dx*3)-lroundf(wx*5),y-lroundf(dy*3)-lroundf(wy*5),color);
}
void drawButton(int x,int w,const char* label) {
  tft.fillRoundRect(x,210,w,27,4,GRID);
  tft.setTextColor(WHITE,GRID);
  tft.drawCentreString(label,x+w/2,216,2);
}
void drawRadar() {
  drawMapBackground();
  for(int r=RADAR_R/3;r<=RADAR_R;r+=RADAR_R/3)tft.drawCircle(CX,CY,r,GRID);
  tft.drawFastHLine(CX-RADAR_R,CY,RADAR_R*2,GRID);
  tft.drawFastVLine(CX,CY-RADAR_R,RADAR_R*2,GRID);
  tft.setTextColor(GREEN,BG);tft.drawCentreString("N",CX,25,1);
  tft.fillCircle(CX,CY,3,GREEN);
  for(int i=0;i<planeCount;i++) {
    if(!visible(planes[i]))continue;
    int x,y;planeToPixel(planes[i],x,y);
    if(!inCircle(x,y,9))continue;
    drawPlaneIcon(x,y,planes[i].heading,i==selectedPlane?YELLOW:GREEN);
    if(i==selectedPlane) {
      tft.setTextColor(YELLOW,BG);
      tft.drawString(planes[i].callsign,constrain(x+8,5,145),constrain(y-12,25,195),1);
    }
  }
}
String compassHeading(float degrees); // used by radar and NEAR
void drawDetails() {
  tft.setTextColor(WHITE,BG);
  tft.drawString("RANGE",200,29,2);
  tft.drawString(String(radarRangeMi,0)+" mi",200,47,2);
  tft.drawString("PLANES",200,74,2);
  tft.drawString(String(visibleCount()),200,92,2);
  tft.drawFastHLine(195,114,120,GRID);
  if(selectedPlane>=0 && selectedPlane<planeCount && visible(planes[selectedPlane])) {
    const Aircraft &a=planes[selectedPlane];
    String c=trimmedCallsign(a.callsign);
    const RouteInfo *r=findRoute(c,a.id);
    tft.setTextColor(YELLOW,BG);tft.drawString(shorten(a.callsign,14),200,118,2);
    tft.setTextColor(WHITE,BG);
    if(r) {
      tft.drawString(shorten(r->airline,19),200,137,1);
      tft.drawString("FROM " + r->origin,200,150,1);
      tft.drawString("TO   " + r->destination,200,162,1);
    } else {
      tft.setTextColor(GRAY,BG);
      tft.drawString(routeBusy==c?"LOOKING UP...":(routePending==c?"ROUTE PENDING":"ROUTE N/A"),200,140,1);
    }
    tft.setTextColor(WHITE,BG);
    tft.drawString(a.hasAltitude?"ALT "+String(a.altitudeFt)+"ft":"ALT --",200,175,1);
    tft.drawString(a.hasSpeed?"SPD "+String(a.speedKt*1.15077945f,0)+"mph":"SPD --",200,185,1);
    tft.drawString(a.hasHeading?"HDG "+compassHeading(a.heading):"HDG --",200,197,1);
  } else {tft.setTextColor(GRAY,BG);tft.drawString("TAP A PLANE",200,130,1);}
}

// List: four-column table, only the five nearest visible aircraft.
// Columns: flight number, origin, destination, distance (statute miles).
constexpr int LIST_ROWS=5;
int visibleIndexAt(int ordinal) {
  int k=0;
  for(int i=0;i<planeCount;i++)if(visible(planes[i])) {
    if(k==ordinal)return i;
    k++;
  }
  return -1;
}
void drawList() {
  const int total=visibleCount();
  listPage=0;
  tft.setTextColor(WHITE,BG);
  tft.drawString("FLIGHTS",5,26,2);
  tft.setTextColor(GRAY,BG);
  tft.drawRightString("TOP 5",315,30,1);

  // Compact header with fixed column starts, sized for 320px screen.
  tft.fillRect(2,45,316,19,0x2104);
  tft.setTextColor(WHITE,0x2104);
  tft.drawString("FLIGHT",6,49,1);
  tft.drawString("FROM",121,49,1);
  tft.drawString("TO",180,49,1);
  tft.drawRightString("DIST (MI)",312,49,1);

  for(int row=0;row<LIST_ROWS;row++) {
    int idx=visibleIndexAt(row);
    if(idx<0)break;
    const Aircraft &a=planes[idx];
    const RouteInfo *r=findRoute(trimmedCallsign(a.callsign),a.id);
    const int y=67+row*21;
    const uint16_t bg=(idx==selectedPlane)?0x2104:BG;
    if(idx==selectedPlane)tft.fillRect(2,y-1,316,20,bg);
    tft.setTextColor(idx==selectedPlane?YELLOW:GREEN,bg);
    tft.drawString(shorten(a.callsign.isEmpty()?a.id:a.callsign,12),6,y,1);
    tft.setTextColor(WHITE,bg);
    tft.drawString(shorten(r?r->origin:"N/A",7),121,y,1);
    tft.drawString(shorten(r?r->destination:"N/A",7),180,y,1);
    tft.drawRightString(String(a.distanceMi,1),312,y,1);
    tft.drawFastHLine(3,y+18,314,0x2104);
  }
  if(total==0){tft.setTextColor(GRAY,BG);tft.drawString("No aircraft in range",8,80,2);}
  tft.setTextColor(GRAY,BG);
  tft.drawString("Nearest 5 | Tap row to highlight on radar",5,199,1);
}
// Convert track degrees (0 = north, clockwise) to an 8-point compass.
String compassHeading(float degrees) {
  static const char* directions[] = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"};
  if (!isfinite(degrees)) return "N/A";
  float normalized = fmodf(degrees, 360.0f);
  if (normalized < 0.0f) normalized += 360.0f;
  int sector = ((int)floorf((normalized + 22.5f) / 45.0f)) % 8;
  return String(directions[sector]);
}

void drawFullDetails() {
  int idx=effectiveSelectedPlane();
  tft.setTextColor(WHITE,BG);
  tft.drawString("NEAREST AIRCRAFT",8,26,2);
  if(idx<0) {
    tft.setTextColor(GRAY,BG);
    tft.drawString("No aircraft in range",8,70,2);
    return;
  }
  const Aircraft &a=planes[idx];
  const String c=trimmedCallsign(a.callsign);
  const RouteInfo *r=findRoute(c,a.id);
  tft.setTextColor(YELLOW,BG);
  tft.drawString("FLIGHT",8,45,1);
  tft.drawString(shorten(a.callsign,18),8,54,2);
  tft.setTextColor(GRAY,BG);
  tft.drawRightString("NEAREST",311,54,1);
  tft.drawFastHLine(8,78,304,GRID);
  tft.setTextColor(WHITE,BG);
  tft.drawString("AIRLINE",8,84,1);
  tft.drawString("FROM",8,112,1);
  tft.drawString("TO",165,112,1);
  tft.setTextColor(GREEN,BG);
  tft.drawString(r?shorten(r->airline,37):shorten(airlineFromCallsign(c),37),8,96,2);
  tft.drawString(r?shorten(r->origin,12):"N/A",8,125,2);
  tft.drawString(r?shorten(r->destination,12):"N/A",165,125,2);
  tft.setTextColor(GRAY,BG);
  tft.drawRightString(r && r->hasRoute?(r->verifiedHex?"ROUTE: AIRLABS / HEX OK":"ROUTE: AIRLABS / UNVERIFIED"):
      (routeBusy==c?"ROUTE: LOOKING UP": "ROUTE: UNKNOWN"),312,141,1);
  tft.drawFastHLine(8,148,304,GRID);
  tft.setTextColor(WHITE,BG);
  tft.drawString("DIST",8,154,1);
  tft.drawString("ALT",86,154,1);
  tft.drawString("SPEED",181,154,1);
  tft.drawString("HDG",277,154,1);
  tft.setTextColor(YELLOW,BG);
  tft.drawString(String(a.distanceMi,1)+"mi",8,167,2);
  tft.drawString(a.hasAltitude?String(a.altitudeFt)+"ft":"N/A",86,167,2);
  tft.drawString(a.hasSpeed?String(a.speedKt*1.15077945f,0)+"mph":"N/A",181,167,2);
  tft.drawString(a.hasHeading?compassHeading(a.heading):"N/A",277,167,2);
  tft.setTextColor(GRAY,BG);
  // AirLabs make/model are shown only after a successful aircraft-matched lookup.
  // If the provider has no make/model, explicitly show N/A.
  String make = (r && r->hasRoute && !r->manufacturer.isEmpty()) ? r->manufacturer : "N/A";
  String model = (r && r->hasRoute && !r->model.isEmpty()) ? r->model : "N/A";
  tft.drawString("MAKE: " + shorten(make,18),8,187,1);
  tft.drawString("MODEL: " + shorten(model,35),8,199,1);
}
void drawUI() {
  tft.fillScreen(BG);
  tft.setTextColor(GREEN,BG);tft.drawString("CYD AIR RADAR",5,3,2);
  tft.setTextColor(dataValid?GREEN:YELLOW,BG);
  tft.drawRightString(statusText,316,5,1);
  if(screenView==LIST_VIEW)drawList();
  else if(screenView==DETAILS_VIEW)drawFullDetails();
  else {drawRadar();drawDetails();}
  if(screenView!=DETAILS_VIEW)drawButton(2,60,"NEXT");
  drawButton(65,60,"- MI");
  drawButton(128,60,"+ MI");
  drawButton(191,60,screenView==DETAILS_VIEW?"RADAR":(screenView==RADAR_VIEW?"LIST":"NEAR"));
  drawButton(254,64,"REFRESH");
  // Radar sidebar uses y=197 for the compass heading.
}

// ================= NETWORK / JSON =================
bool fetchAircraft() {
  if(WiFi.status()!=WL_CONNECTED){statusText="NO WIFI";dataValid=false;return false;}
  // API radius is nautical miles, display radius is statute miles.
  int nm=constrain((int)ceilf(FETCH_RADIUS_MI/1.15077945f),1,250);
  String url="https://opendata.adsb.fi/api/v3/lat/"+String(HOME_LAT,5)+
             "/lon/"+String(HOME_LON,5)+"/dist/"+String(nm);
  Serial.println("Request: "+url);
  Serial.printf("Free heap before request: %u\n",ESP.getFreeHeap());
  WiFiClientSecure client;client.setInsecure(); // Prototype only: no certificate validation.
  HTTPClient http;
  http.useHTTP10(true);
  http.setConnectTimeout(5000);
  http.setTimeout(10000);
  Serial.println("[ADSB] Starting request...");
  if(!http.begin(client,url)){Serial.println("[ADSB] http.begin failed");statusText="HTTP INIT";dataValid=false;return false;}
  http.addHeader("User-Agent","CYD-AircraftRadar/1.0 (personal project)");
  http.addHeader("Accept","application/json");
  Serial.println("[ADSB] Sending GET...");
  uint32_t getStart=millis();
  int code=http.GET();
  Serial.printf("[ADSB] HTTP status: %d, bytes: %d, GET ms: %lu\n",code,http.getSize(),(unsigned long)(millis()-getStart));
  if(code!=HTTP_CODE_OK) {
    statusText="HTTP "+String(code);dataValid=false;
    if(code>0)Serial.println(http.getString().substring(0,200));
    http.end();return false;
  }
  // Filter retained JSON fields to limit document allocation.
  JsonDocument filter;
  filter["ac"][0]["hex"]=true;
  filter["ac"][0]["flight"]=true;
  filter["ac"][0]["lat"]=true;
  filter["ac"][0]["lon"]=true;
  filter["ac"][0]["track"]=true;
  filter["ac"][0]["gs"]=true;
  filter["ac"][0]["alt_baro"]=true;
  Serial.printf("[ADSB] Heap before download: %u; largest block: %u\n",
                ESP.getFreeHeap(),ESP.getMaxAllocHeap());
  Serial.println("[ADSB] Downloading response...");
  uint32_t downloadStart=millis();
  String payload=http.getString(); // HTTPClient handles response framing.
  Serial.printf("[ADSB] Download finished in %lu ms, bytes=%u\n",
                (unsigned long)(millis()-downloadStart),(unsigned)payload.length());
  Serial.printf("[ADSB] Heap after download: %u; largest block: %u\n",
                ESP.getFreeHeap(),ESP.getMaxAllocHeap());
  if(!payload.isEmpty()) {
    Serial.print("[ADSB] JSON beginning: ");
    Serial.println(payload.substring(0,100));
    Serial.print("[ADSB] JSON ending: ");
    Serial.println(payload.substring(payload.length()>100?payload.length()-100:0));
  }
  http.end();
  if(payload.isEmpty()) {
    Serial.println("[ADSB] Empty HTTP body");
    statusText="EMPTY BODY";dataValid=false;return false;
  }
  JsonDocument doc;
  DeserializationError err=deserializeJson(doc,payload,DeserializationOption::Filter(filter));
  Serial.printf("[ADSB] JSON parse result: %s\n",err.c_str());
  payload=String();
  if(err) {
    Serial.printf("[ADSB] JSON error: %s; heap=%u\n",err.c_str(),ESP.getFreeHeap());
    statusText="JSON ERROR";dataValid=false;return false;
  }
  JsonArray arr=doc["ac"].as<JsonArray>();
  if(arr.isNull()) {Serial.println("Missing 'ac' array");statusText="NO DATA";dataValid=false;return false;}
  Serial.printf("API aircraft: %u; free heap: %u\n",(unsigned)arr.size(),ESP.getFreeHeap());
  String oldId=(selectedPlane>=0 && selectedPlane<planeCount)?planes[selectedPlane].id:"";
  int count=0;
  for(JsonObject o:arr) {
    if(o["lat"].isNull() || o["lon"].isNull())continue;
    Aircraft a;
    a.id=o["hex"].as<String>();
    a.callsign=o["flight"].as<String>();a.callsign.trim();
    if(a.callsign.isEmpty())a.callsign=a.id;
    if(a.callsign.isEmpty())a.callsign="UNKNOWN";
    a.hasHeading=!o["track"].isNull();a.heading=o["track"]|0.0f;
    a.hasSpeed=!o["gs"].isNull();a.speedKt=o["gs"]|0.0f;
    a.hasAltitude=o["alt_baro"].is<int>();
    a.altitudeFt=a.hasAltitude?o["alt_baro"].as<int>():0;
    locateAircraft(o["lat"].as<float>(),o["lon"].as<float>(),a);
    if(a.distanceMi>FETCH_RADIUS_MI)continue;
    int pos=0;while(pos<count && incoming[pos].distanceMi<=a.distanceMi)pos++;
    if(pos>=MAX_PLANES)continue;
    if(count<MAX_PLANES)count++;
    for(int j=count-1;j>pos;j--)incoming[j]=incoming[j-1];
    incoming[pos]=a;
  }
  planeCount=count;
  for(int i=0;i<count;i++)planes[i]=incoming[i];
  selectedPlane=-1;
  if(!oldId.isEmpty())for(int i=0;i<count;i++)if(planes[i].id==oldId){selectedPlane=i;break;}
  statusText="LIVE";dataValid=true;
  queueSelectedRoute();
  Serial.printf("Retained nearest aircraft: %d\n",count);
  return true;
}

// ================= AIRLABS SINGLE-ATTEMPT LOOKUP =================
// A flight is identified by callsign AND ICAO hex. Every attempt (including
// timeout, HTTP error, malformed JSON, missing route, hex mismatch) is recorded
// BEFORE the network request. No retries during this boot, even on errors.
void lookupRoute(const String &callsign, const String &targetHex="") {
  if(WiFi.status()!=WL_CONNECTED || !ENABLE_ROUTE_LOOKUP)return;
  int idx=-1;
  for(int i=0;i<planeCount;i++)
    if(trimmedCallsign(planes[i].callsign)==callsign && validCallsign(planes[i]) &&
       (targetHex.isEmpty() || normalizedHex(planes[i].id)==targetHex)){idx=i;break;}
  if(idx<0)return;
  const Aircraft &a=planes[idx];
  if(findRoute(callsign,a.id)) {Serial.printf("[AIRLABS] CACHE HIT (including previous failures): %s hex=%s\n",callsign.c_str(),a.id.c_str());return;}
  if(routeCacheCount>=ROUTE_CACHE_SIZE){Serial.println("[AIRLABS] Ledger full; no more requests");return;}
  if(String(AIRLABS_API_KEY)=="YOUR_AIRLABS_API_KEY" || strlen(AIRLABS_API_KEY)==0) {
    Serial.println("[AIRLABS] Configure AIRLABS_API_KEY first");return;
  }
  RouteInfo r;
  r.callsign=callsign;
  r.hex=normalizedHex(a.id);
  r.airline=airlineFromCallsign(callsign);
  r.origin="N/A";r.destination="N/A";
  r.attempted=true;
  storeRoute(r); // MUST precede GET to enforce at-most-once, including failures.
  int slot=routeCacheCount-1;
  routeBusy=callsign;
  drawUI();
  Serial.printf("[AIRLABS] Attempt %d/%d: %s hex=%s\n",routeCacheCount,
                ROUTE_CACHE_SIZE,callsign.c_str(),r.hex.c_str());
  WiFiClientSecure client;
  client.setInsecure(); // Prototype only: TLS certificate is not verified.
  HTTPClient http;
  http.useHTTP10(true);
  http.setConnectTimeout(5000);
  http.setTimeout(10000);
  String url="https://airlabs.co/api/v9/flight?flight_icao="+callsign+
             "&api_key="+String(AIRLABS_API_KEY);
  if(http.begin(client,url)) {
    http.addHeader("Accept","application/json");
    int code=http.GET();
    Serial.printf("[AIRLABS] HTTP %d for %s\n",code,callsign.c_str());
    if(code==HTTP_CODE_OK) {
      String body=http.getString();
      JsonDocument doc;
      DeserializationError err=deserializeJson(doc,body);
      if(!err && doc["response"].is<JsonObject>()) {
        JsonObject f=doc["response"].as<JsonObject>();
        String apiFlight=f["flight_icao"] | "";
        apiFlight=trimmedCallsign(apiFlight);
        String apiHex=f["hex"] | "";
        apiHex=normalizedHex(apiHex);
        String from=f["dep_iata"] | "";
        String to=f["arr_iata"] | "";
        from.trim();to.trim();from.toUpperCase();to.toUpperCase();
        // Reject conflicting aircraft identities and flight numbers.
        bool flightMatches=apiFlight==callsign;
        bool hexMatches=!apiHex.isEmpty() && apiHex==r.hex;
        bool hexConflict=!apiHex.isEmpty() && !r.hex.isEmpty() && apiHex!=r.hex;
        if(flightMatches && !hexConflict && from.length()==3 &&
           to.length()==3 && from!=to) {
          routeCache[slot].origin=from;
          routeCache[slot].destination=to;
          routeCache[slot].hasRoute=true;
          routeCache[slot].verifiedHex=hexMatches;
          String airline=f["airline_name"] | "";
          if(!airline.isEmpty())routeCache[slot].airline=airline;
          routeCache[slot].manufacturer=f["manufacturer"] | "";
          routeCache[slot].model=f["model"] | "";
          routeCache[slot].flightStatus=f["status"] | "";
          Serial.printf("[AIRLABS] %s %s -> %s hex=%s\n",callsign.c_str(),
                        from.c_str(),to.c_str(),hexMatches?"MATCH":"NOT PROVIDED");
        } else {
          Serial.printf("[AIRLABS] Rejected: flightMatch=%d hexConflict=%d from=%s to=%s\n",
                        flightMatches,hexConflict,from.c_str(),to.c_str());
        }
      } else Serial.printf("[AIRLABS] JSON/API error: %s\n",err.c_str());
    }
    http.end();
  } else Serial.println("[AIRLABS] HTTP begin failed");
  // No retries, regardless of result. Retain the entry until reboot.
  routeBusy="";
  drawUI();
}

// ================= INPUT =================
bool readTouch(int &x,int &y) {
  if(!touch.touched())return false;
  TS_Point p=touch.getPoint();
  x=map(constrain(p.y,RAW_Y_MIN,RAW_Y_MAX),RAW_Y_MIN,RAW_Y_MAX,0,319);
  y=map(constrain(p.x,RAW_X_MIN,RAW_X_MAX),RAW_X_MAX,RAW_X_MIN,0,239);
  return true;
}
// A radar selection can come from tapping a plane, NEXT, or a List row.
// All three must use the same cache/priority behavior.
void requestSelectedRadarRoute(const char *source) {
  if(screenView!=RADAR_VIEW || selectedPlane<0 || selectedPlane>=planeCount)return;
  const Aircraft &a=planes[selectedPlane];
  String c=trimmedCallsign(a.callsign);
  Serial.printf("[AIRLABS] %s SELECT: %s hex=%s\n",source,c.c_str(),a.id.c_str());
  if(!validCallsign(a)) {
    Serial.println("[AIRLABS] SKIP: no usable callsign");
  } else if(const RouteInfo *cached=findRoute(c,a.id)) {
    Serial.printf("[AIRLABS] CACHE HIT: %s result=%s (no new request)\n",
                  c.c_str(),cached->hasRoute?"ROUTE OK":"PREVIOUS ATTEMPT / N/A");
  } else if(routeCacheCount>=ROUTE_CACHE_SIZE) {
    Serial.println("[AIRLABS] SKIP: attempt ledger full");
  } else {
    radarPriorityCall=c;
    radarPriorityHex=normalizedHex(a.id);
    routePending="";
    Serial.printf("[AIRLABS] RADAR PRIORITY QUEUED: %s hex=%s\n",c.c_str(),a.id.c_str());
  }
}
// Cycle through aircraft currently inside the displayed radar range.
// Aircraft are already stored nearest-first by fetchAircraft().
void selectNextPlane() {
  if(planeCount<=0){selectedPlane=-1;return;}
  for(int offset=1;offset<=planeCount;offset++) {
    int candidate=(selectedPlane+offset)%planeCount;
    if(candidate<0)candidate+=planeCount;
    if(visible(planes[candidate])) {
      selectedPlane=candidate;
      // Stay in the current view; keep the selected plane on the list page.
      int ordinal=0;
      for(int i=0;i<candidate;i++)if(visible(planes[i]))ordinal++;
      listPage=ordinal/LIST_ROWS;
      Serial.printf("[SELECT] Aircraft %d/%d: %s\n",
                    selectedPlane+1,planeCount,planes[selectedPlane].callsign.c_str());
      requestSelectedRadarRoute("NEXT");
      return;
    }
  }
  selectedPlane=-1;routePending="";
}
void handleTouch(int x,int y) {
  Serial.printf("Touch: %d,%d\n",x,y);
  if(y>=210) {
    if(x<64){
      if(screenView!=DETAILS_VIEW)selectNextPlane();
    }
    else if(x<127){radarRangeMi=min(MAX_RANGE_MI,radarRangeMi*2.0f);selectedPlane=-1;routePending="";listPage=0;}
    else if(x<190){radarRangeMi=max(MIN_RANGE_MI,radarRangeMi/2.0f);selectedPlane=-1;routePending="";listPage=0;}
    else if(x<253) {
      screenView=screenView==DETAILS_VIEW?RADAR_VIEW:(screenView==RADAR_VIEW?LIST_VIEW:DETAILS_VIEW);
      queueSelectedRoute();
    } else fetchRequested=true;
    drawUI();return;
  }
  if(screenView==LIST_VIEW) {
    if(y>=67 && y<193) {
      int row=(y-67)/21;
      int idx=visibleIndexAt(row);
      if(idx>=0) {
        // A list-row tap highlights this aircraft on the radar.
        // The list itself queues AirLabs for its five rows; do not
        // spend an extra request just because the radar was opened.
        selectedPlane=idx;
        screenView=RADAR_VIEW;
        routePending="";
        Serial.printf("[SELECT] List -> radar: %s\n",planes[idx].callsign.c_str());
        requestSelectedRadarRoute("LIST");
      }

    }
    drawUI();return;
  }
  if(screenView==DETAILS_VIEW) {
    // Tap the details screen to return to the radar.
    screenView=RADAR_VIEW;drawUI();return;
  }
  int best=-1;float bestD2=16*16;
  for(int i=0;i<planeCount;i++) {
    if(!visible(planes[i]))continue;
    int px,py;planeToPixel(planes[i],px,py);
    if(!inCircle(px,py,9))continue;
    float dx=x-px,dy=y-py,d2=dx*dx+dy*dy;
    if(d2<bestD2){bestD2=d2;best=i;}
  }
  if(best>=0){
    selectedPlane=best;
    requestSelectedRadarRoute("RADAR TAP");
    Serial.printf("[SELECT] Radar tap: %s\n",planes[best].callsign.c_str());
  }
  drawUI();
}
void connectWiFi() {
  if(WiFi.status()==WL_CONNECTED)return;
  statusText="CONNECTING";drawUI();
  WiFi.mode(WIFI_STA);WiFi.begin(WIFI_SSID,WIFI_PASS);
  uint32_t start=millis();
  while(WiFi.status()!=WL_CONNECTED && millis()-start<15000)delay(250);
  if(WiFi.status()==WL_CONNECTED) {
    statusText="WIFI OK";Serial.print("IP: ");Serial.println(WiFi.localIP());
  }else statusText="NO WIFI";
  drawUI();
}
void setup() {
  Serial.begin(115200);delay(500);
  pinMode(21,OUTPUT);digitalWrite(21,HIGH);
  tft.init();tft.setRotation(1);tft.invertDisplay(false);
  SPI.begin(25,39,32,TOUCH_CS);
  Serial.println(touch.begin()?"Touch initialized":"Touch init failed");
  touch.setRotation(0);
  drawUI();connectWiFi();
  Serial.printf("CYD radar: refresh %lu ms, max %.0f mi\n",(unsigned long)REFRESH_MS,MAX_RANGE_MI);
}
void loop() {
  static uint32_t lastWiFiCheck=0;
  if(millis()-lastWiFiCheck>=15000) {
    lastWiFiCheck=millis();
    if(WiFi.status()!=WL_CONNECTED)connectWiFi();
  }
  uint32_t now=millis();
  bool due=!hasAttemptedFetch || (int32_t)(now-nextFetch)>=0;
  bool manual=fetchRequested && (!hasAttemptedFetch || now-lastFetch>=MIN_REQUEST_GAP_MS);
  if(due || manual) {
    fetchRequested=false;hasAttemptedFetch=true;lastFetch=now;
    statusText="LOADING";drawUI();
    bool ok=fetchAircraft();
    Serial.printf("[ADSB] Fetch complete: %s; status=%s\n",ok?"OK":"FAILED",statusText.c_str());
    nextFetch=millis()+(ok?max(REFRESH_MS,MIN_REQUEST_GAP_MS):ERROR_RETRY_MS);
    drawUI();
  }
  // Read touch BEFORE background scheduling: a radar tap gets priority immediately.
  static bool wasTouching=false;
  int x,y;bool touching=readTouch(x,y);
  if(touching && !wasTouching)handleTouch(x,y);
  wasTouching=touching;

  // Drop a priority request if the target has vanished or was already attempted.
  if(!radarPriorityCall.isEmpty()) {
    bool present=false;
    for(int i=0;i<planeCount;i++) {
      if(trimmedCallsign(planes[i].callsign)==radarPriorityCall &&
         normalizedHex(planes[i].id)==radarPriorityHex) {
        present=true;
        if(const RouteInfo *r=findRoute(radarPriorityCall,radarPriorityHex)) {
          Serial.printf("[AIRLABS] PRIORITY CACHE HIT: %s (%s)\n",
                        radarPriorityCall.c_str(),r->hasRoute?"OK":"N/A");
          radarPriorityCall="";radarPriorityHex="";
        }
        break;
      }
    }
    if(!present) {
      Serial.println("[AIRLABS] Priority aircraft no longer present; dropping pending request");
      radarPriorityCall="";radarPriorityHex="";
    }
  }

  // The list automatically enriches only the nearest five; never preempt a radar tap.
  if(radarPriorityCall.isEmpty() && screenView==LIST_VIEW &&
     routePending.isEmpty() && ENABLE_ROUTE_LOOKUP) {
    for(int row=0;row<LIST_ROWS;row++) {
      int idx=visibleIndexAt(row);
      if(idx<0)break;
      String c=trimmedCallsign(planes[idx].callsign);
      if(!validCallsign(planes[idx]))continue;
      if(!findRoute(c,planes[idx].id) && routeCacheCount<ROUTE_CACHE_SIZE) {
        routePending=c;
        Serial.printf("[AIRLABS] LIST QUEUED: %s\n",c.c_str());
        break;
      }
    }
  }
  // Keep the nearest-aircraft lookup behavior on the NEAR page.
  if(radarPriorityCall.isEmpty() && screenView==DETAILS_VIEW &&
     routePending.isEmpty())queueSelectedRoute();

  if(WiFi.status()==WL_CONNECTED && ENABLE_ROUTE_LOOKUP &&
     (lastRouteLookup==0 || millis()-lastRouteLookup>=3000)) {
    if(!radarPriorityCall.isEmpty()) {
      String c=radarPriorityCall;
      String hex=radarPriorityHex;
      radarPriorityCall="";radarPriorityHex="";
      // lookupRoute locates the aircraft by callsign; validate the hex before calling.
      bool stillPresent=false;
      for(int i=0;i<planeCount;i++)if(trimmedCallsign(planes[i].callsign)==c &&
          normalizedHex(planes[i].id)==hex){stillPresent=true;break;}
      if(stillPresent) {
        Serial.printf("[AIRLABS] RADAR PRIORITY REQUEST: %s hex=%s\n",c.c_str(),hex.c_str());
        lastRouteLookup=millis();
        lookupRoute(c,hex);
        Serial.println("[AIRLABS] RADAR REDRAW");
        drawUI();
      }
    } else if(!routePending.isEmpty()) {
      String c=routePending;
      routePending="";
      lastRouteLookup=millis();
      Serial.printf("[AIRLABS] BACKGROUND REQUEST: %s\n",c.c_str());
      lookupRoute(c);
    }
  }
  delay(30);
}
