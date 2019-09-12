

sql_Create_Survey <- '

CREATE TABLE SURVEY (
    ID          INTEGER PRIMARY KEY
                        UNIQUE,
    BUSINESS_ID TEXT,
    NAME        TEXT,
    SURVEY_TYPE TEXT,
    MAP_SCALE   INTEGER,
    VOLUNTEER   INTEGER,
    PUBLIC_DATA TEXT
);
'

sql_Create_SITE <- '
CREATE TABLE SITE (
    ID                   INTEGER PRIMARY KEY
                                 NOT NULL
                                 UNIQUE,
    SURVEY_ID            INTEGER REFERENCES SURVEY (ID) ON DELETE CASCADE
                                                        ON UPDATE CASCADE,
    SITE_NO              TEXT,
    DATE_ENTERED         TEXT,
    PHOTO_FILMNUMBER     TEXT,
    PHOTO_YEAR           INTEGER,
    PHOTO_RUN            TEXT,
    PHOTO_FRAMENUMBER    TEXT,
    PHOTO_REFEAST        INTEGER,
    PHOTO_REFNORTH       INTEGER,
    LANDUSE              TEXT,
    DATE_DESCRIBED       INTEGER,
    LANDUSE_LUMP_2007    TEXT,
    GDA94_LATITUDE       REAL,
    GDA94_LONGITUDE      TEXT,
    ANALYTICAL_SITE      TEXT,
    ANALYTICAL_SITE_TYPE TEXT,
    CONSIDERED_ACCURACY  TEXT,
    POSITIONAL_METHOD    TEXT,
    PHOTO_REFERENCE      REAL,
    SiteType             TEXT,
    Comment              TEXT,
    LocationAccuracy     TEXT,
    LocationMethod       TEXT,
    ProjectName          TEXT
);

'

sql_Create_SOIL <- 'CREATE TABLE SOIL (
    ID                    INTEGER PRIMARY KEY
                                  UNIQUE
                                  NOT NULL,
    SITE_ID               INTEGER REFERENCES SITE (ID) ON DELETE CASCADE
                                                       ON UPDATE CASCADE,
    SOIL_OBSERVATION_TYPE TEXT,
    FREEWATER_CODE        TEXT,
    SOIL_EXPOSURE_STOP_BY TEXT,
    ASC_ORD               TEXT,
    ASC_SUBORDER          TEXT,
    ASC_GREATER_GROUP     TEXT,
    ASC_SUB_GROUP         TEXT,
    PPF                   TEXT,
    GSG_CODE              TEXT,
    CONFIDENCE            INTEGER,
    FREEWATER_DEPTH       REAL,
    EXPOSURE_DEPTH        REAL,
    ASC_FAM1              TEXT,
    ASC_FAM2              TEXT,
    ASC_FAM3              TEXT,
    ASC_FAM4              TEXT,
    ASC_FAM5              TEXT,
    COMMENTS_SOIL         TEXT,
    WRB_2006              TEXT,
    WRB_2006_PREFIX       TEXT,
    WRB_2006_SUFFIX       TEXT,
    WRB_2006_SPECIFIER    TEXT,
    SOIL_FAMILY           TEXT,
    SPC                   TEXT,
    ASC_VERSION           TEXT,
    SOIL_FAMILY_NEW       TEXT,
    SOIL_SERIES           TEXT
);

'

sql_Create_BULK_DENSITY <- '
CREATE TABLE BULK_DENSITY_DATA (
    SOIL_ID               INTEGER REFERENCES SOIL (ID) ON DELETE CASCADE
                                                       ON UPDATE CASCADE,
    ID                    INTEGER,
    LOWER_DEPTH           REAL,
    UPPER_DEPTH           REAL,
    SAMPLE_1_UNKNOWN      REAL,
    SAMPLE_2_UNKNOWN      INTEGER,
    SAMPLE_3_UNKNOWN      INTEGER,
    SAMPLE_4_UNKNOWN      INTEGER,
    SAMPLE_1_SMALL        REAL,
    SAMPLE_2_SMALL        REAL,
    SAMPLE_3_SMALL        INTEGER,
    SAMPLE_4_SMALL        INTEGER,
    SAMPLE_1_LARGE        REAL,
    SAMPLE_2_LARGE        REAL,
    SAMPLE_3_LARGE        INTEGER,
    SAMPLE_4_LARGE        INTEGER,
    SAMPLE_1_DISPLACEMENT REAL,
    SAMPLE_2_DISPLACEMENT REAL,
    SAMPLE_3_DISPLACEMENT REAL,
    SAMPLE_4_DISPLACEMENT INTEGER,
    HORIZON_CODE          TEXT,
    SAMPLE_DATE           TEXT
);
'

sql_Create_HORIZON<- 'CREATE TABLE HORIZON (
  SOIL_ID                   INTEGER REFERENCES SOIL (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  ID                        INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  HORIZON_CODE              TEXT,
  UPPER_DEPTH               REAL,
  LOWER_DEPTH               REAL,
  BOUNDARY_DISTINCT         TEXT,
  BOUNDARY_SHAPE            TEXT,
  COLOUR_HUE_VALUE_CHROMA_D TEXT,
  COLOUR_HUE_VALUE_CHROMA_M TEXT,
  PH                        REAL,
  PH_METHOD                 INTEGER,
  EFFERVESCENCE             TEXT,
  TEXTURE_GRADE             TEXT,
  TEXTURE_QUALIFICATION_1   TEXT,
  SOIL_WATER_STATUS         TEXT,
  STRENGTH                  TEXT,
  STICKINESS                INTEGER,
  PLASTIC_TYPE              TEXT,
  PLASTIC_DEGREE            INTEGER,
  ELECTRIC_CONDUCT          REAL,
  TEXTURE_MODIFIER          TEXT,
  TEXTURE_QUALIFICATION_2   TEXT,
  TYPE                      TEXT,
  ABUNDANCE                 INTEGER,
  DISTINCTIVENESS           TEXT,
  CRACK_WIDTH               INTEGER,
  PORE_ABUNDANCE            INTEGER,
  PORE_DIAMETER             INTEGER,
  PORE_TYPE                 TEXT,
  ROOT_ABUNDANCE            TEXT,
  ROOT_SIZE                 INTEGER,
  PANS_CEMENTATION          INTEGER,
  PANS_TYPE                 TEXT,
  PANS_CONTINUITY           TEXT,
  PANS_STRUCTURE            TEXT
);'

sql_Create_CHEMICAL <- 'CREATE TABLE CHEMICAL (
  SOIL_ID                       INTEGER REFERENCES SOIL (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  SAMPLE_DATE                   TEXT,
  U_DEPTH                       REAL,
  L_DEPTH                       REAL,
  PH                            REAL,
  EC                            REAL,
  Na                            REAL,
  K                             REAL,
  CA                            REAL,
  MG                            REAL,
  EX_ACIDITY_AL                 REAL,
  EX_ACIDITY_H                  REAL,
  CEC                           REAL,
  ECEC                          REAL,
  BASE_STATUS                   REAL,
  ORG_CARBON_PC                 REAL,
  CU_TOTAL                      REAL,
  PB_TOTAL                      REAL,
  ZN_TOTAL                      REAL,
  MN_TOTAL                      REAL,
  CU_EXTRACTABLE                REAL,
  PB_EXTRACTABLE                REAL,
  ZN_EXTRACTABLE                REAL,
  MN_EXTRACTABLE                REAL,
  FE_EXTRACTABLE                REAL,
  N_TOTAL                       REAL,
  P_TOTAL                       REAL,
  K_TOTAL                       REAL,
  S_TOTAL                       REAL,
  P_EXTRACTABLE                 REAL,
  K_EXTRACTABLE                 REAL,
  S_EXTRACTABLE                 REAL,
  CL_SOLUABLE                   REAL,
  CLAY                          REAL,
  SILT                          REAL,
  FINE_SAND                     REAL,
  COARSE_SAND                   REAL,
  GRAVEL                        REAL,
  GRAV_MOIST_01BAR              REAL,
  GRAV_MOIST_05BAR              REAL,
  GRAV_MOIST_15BAR              REAL,
  SM_CLAY                       INTEGER,
  V_CLAY                        INTEGER,
  I_CLAY                        REAL,
  K_CLAY                        REAL,
  Q_CLAY                        INTEGER,
  HG_CLAY                       INTEGER,
  O_CLAY                        INTEGER,
  BULK_DENSITY                  REAL,
  AIRDRYMOISTURECONTENT         REAL,
  WATERHOLDCAP_01TO15BAR        REAL,
  WATERHOLDCAP_03TO15BAR        REAL,
  ID                            INTEGER,
  HQ_CLAY                       REAL,
  HORIZON_CODE                  TEXT,
  PH_CACL2                      REAL,
  SAMPLE_PREPARATION            TEXT,
  ORG_MATTER_PC                 REAL,
  BULK_DENSITY_SMALL_RING       INTEGER,
  BULK_DENSITY_LARGE_RING       INTEGER,
  BULK_DENSITY_DISPLACEMENT     INTEGER,
  SATURATED_HYDRAULIC_CONDUCT   REAL,
  UNSATURATED_HYDRAULIC_CONDUCT INTEGER,
  SOIL_GROUP                    INTEGER,
  NA_DUP                        REAL,
  K_DUP                         REAL,
  CA_DUP                        REAL,
  CEC_DUP                       REAL,
  MG_DUP                        REAL,
  P_EXTRACTABLE_DUP             REAL,
  R1                            REAL,
  GYPSUM                        REAL,
  N_DUP                         REAL,
  AL_SAT                        REAL,
  R2                            REAL,
  FREEFE                        INTEGER,
  FREEAL                        INTEGER
);

'

sql_Create_AGGRADATION <- 'CREATE TABLE AGGRADATION (
  ID          INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  LANDFORM_ID INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  STATE       TEXT,
  DEPTH       REAL,
  TYPE        TEXT
);
'

sql_Create_CHEMICAL_METHODS <- 'CREATE TABLE CHEMICAL_METHODS (
  SURVEY_ID                 INTEGER REFERENCES SURVEY (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  PH                        TEXT,
  EC                        TEXT,
  Na                       TEXT,
  K                         TEXT,
  CA                        TEXT,
  MG                        TEXT,
  EX_ACIDITY_AL             TEXT,
  EX_ACIDITY_H              TEXT,
  CEC                       TEXT,
  ECEC                      TEXT,
  BASE_STATUS               INTEGER,
  ORG_CARBON_PC             TEXT,
  CU_TOTAL                  TEXT,
  PB_TOTAL                  TEXT,
  ZN_TOTAL                  TEXT,
  MN_TOTAL                  TEXT,
  CU_EXTRACTABLE            TEXT,
  PB_EXTRACTABLE            TEXT,
  ZN_EXTRACTABLE            TEXT,
  MN_EXTRACTABLE            TEXT,
  FE_EXTRACTABLE            TEXT,
  N_TOTAL                   TEXT,
  P_TOTAL                   TEXT,
  K_TOTAL                   TEXT,
  S_TOTAL                   TEXT,
  P_EXTRACTABLE             TEXT,
  K_EXTRACTABLE             TEXT,
  S_EXTRACTABLE             TEXT,
  CL_SOLUABLE               TEXT,
  CLAY                      TEXT,
  SILT                      TEXT,
  FINE_SAND                 TEXT,
  COARSE_SAND               TEXT,
  GRAVEL                    TEXT,
  GRAV_MOIST_01BAR          TEXT,
  GRAV_MOIST_05BAR          TEXT,
  GRAV_MOIST_15BAR          TEXT,
  SM_CLAY                   INTEGER,
  V_CLAY                    INTEGER,
  I_CLAY                    INTEGER,
  K_CLAY                    INTEGER,
  Q_CLAY                    INTEGER,
  HG_CLAY                   INTEGER,
  O_CLAY                    INTEGER,
  BULK_DENSITY              REAL,
  AIRDRYMOISTURECONTENT     TEXT,
  HQ_CLAY                   INTEGER,
  WATERHOLDCAP_01TO15BAR    INTEGER,
  WATERHOLDCAP_03TO15BAR    INTEGER,
  PARTICLE_SIZE             TEXT,
  PH_CACL2                  TEXT,
  SAT_HYDRAULIC_CONDUCT_M   INTEGER,
  UNSAT_HYDRAULIC_CONDUCT_M INTEGER,
  NA_DUP                    TEXT,
  K_DUP                     TEXT,
  CA_DUP                    TEXT,
  CEC_DUP                   TEXT,
  MG_DUP                    TEXT,
  P_EXTRACTABLE_DUP         TEXT,
  R1                        TEXT,
  GYPSUM                    INTEGER,
  N_DUP                     TEXT,
  AL_SAT                    TEXT,
  R2                        TEXT,
  FREEFE                    INTEGER,
  FREEAL                    INTEGER
);

'

sql_Create_COARSE_FRAGMENTS <- 'CREATE TABLE COARSE_FRAGMENTS (
    ID         INTEGER PRIMARY KEY
                       UNIQUE
                       NOT NULL,
    HORIZON_ID INTEGER REFERENCES HORIZON (ID) ON DELETE CASCADE
                                               ON UPDATE CASCADE,
    ABUNDANCE  REAL,
    FRAG_SIZE  REAL,
    SHAPE      TEXT,
    LITHOLOGY  TEXT
);
'


sql_Create_COARSE_FRAGMENTS_LANDFORM <- 'CREATE TABLE COARSE_FRAGMENTS_LANDFORM (
    ID          INTEGER PRIMARY KEY
                        UNIQUE
                        NOT NULL,
    LANDFORM_ID INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
                                                 ON UPDATE CASCADE,
    ABUNDANCE   REAL,
    FRAG_SIZE   REAL,
    SHAPE       TEXT,
    LITHOLOGY   TEXT
);
'


sql_Create_DRAINAGE <- 'CREATE TABLE DRAINAGE (
    ID          INTEGER PRIMARY KEY
                        UNIQUE
                        NOT NULL,
    HORIZON_ID  INTEGER REFERENCES HORIZON (ID) ON DELETE CASCADE
                                                ON UPDATE CASCADE,
    DRAINAGE_ID INTEGER
);
'


sql_Create_EROSION<- 'CREATE TABLE EROSION (
    ID          INTEGER PRIMARY KEY
                        UNIQUE
                        NOT NULL,
    LANDFORM_ID INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
                                                 ON UPDATE CASCADE,
    STATE       TEXT,
    TYPE        TEXT,
    GULLY_DEPTH REAL,
    GULLY_WIDTH REAL,
    EXTENT_PC   REAL,
    DEGREE      TEXT
);
'

sql_Create_LANDFORM<- 'CREATE TABLE LANDFORM (
  ID                            INTEGER PRIMARY KEY
  UNIQUE,
  SITE_ID                       INTEGER REFERENCES SITE (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  SLOPE_CLASS                   TEXT,
  SLOPE_PERCENTAGE              REAL,
  MORPHOLOGICAL_TYPE            TEXT,
  SLOPE_INCLINATION             TEXT,
  ASPECT                        REAL,
  SLOPE_EVAL_METHOD             TEXT,
  PERMEABILITY                  INTEGER,
  RUNOFF                        INTEGER,
  LANDFORM_ELEMENT              TEXT,
  ELEMENT_MODE_OF_GEOMORPHOLOGY TEXT,
  LANDFORM_PATTERN              TEXT,
  MODAL_SLOPE                   TEXT,
  EROSIONAL_LANDFORM_PATTERN    TEXT,
  PATTERN_MODE_GEOMORPH_ACT     TEXT,
  STREAM_CHANNEL_SPACING        TEXT,
  STREAM_CHANNEL_DEV            TEXT,
  STREAM_CHANNEL_WIDTH          REAL,
  STREAM_CHANNEL_DEPTH          REAL,
  STREAM_WISE_CHANNEL_PATTERN   TEXT,
  MOISTURE_STATUS               TEXT,
  DRAINAGE                      INTEGER,
  PATTERN_RELIEF_METERS         REAL,
  LAND_SYSTEM                   TEXT,
  MAP_UNIT                      TEXT,
  GEOLOGY_REDUNDANT             TEXT,
  GEOLOGY_MAPSHEET_SCALE        TEXT,
  GEOLOGY_MAPSHEET_NAME         TEXT,
  GEOLOGY_SYMBOL                TEXT,
  DIST_TYPE                     TEXT,
  COMMENTS                      TEXT,
  FREQUENCY                     INTEGER,
  DURATION                      INTEGER,
  DEPTH_VAL                     REAL,
  REGOLITH                      TEXT,
  MODAL_SLOPE_PERCENTAGE        REAL
);
'

sql_Create_LANDS_STAFF <- '
CREATE TABLE LANDS_STAFF (
  FIRST_NAME  TEXT,
  SURNAME     TEXT,
  ID          INTEGER,
  EXPORT_NAME TEXT
);'

sql_Create_MICRORELIEF <- '
CREATE TABLE MICRORELIEF (
  ID                       INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  LANDFORM_ID              INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  TYPE                     TEXT,
  PROP_GILGAI_COMP         TEXT,
  VERT_INTERVAL            REAL,
  HORIZ_INTERVAL           REAL,
  COMP_MICRORELIEF_SAMPLED TEXT
);
'

sql_Create_MOTTLE <- '
CREATE TABLE MOTTLE (
  ID          INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  HORIZON_ID  INTEGER REFERENCES HORIZON (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  TYPE        TEXT,
  ABUNDANCE   REAL,
  MOTTLE_SIZE INTEGER,
  CONTRAST    TEXT,
  COLOUR      TEXT
);
'

sql_Create_OBSERVER <- '
CREATE TABLE OBSERVER (
  SITE_ID  INTEGER,
  STAFF_ID INTEGER
);
'

sql_Create_PERMEABILITY <- '
CREATE TABLE PERMEABILITY (
  ID              INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  HORIZON_ID      INTEGER REFERENCES HORIZON (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  PERMEABILITY_ID INTEGER
);
'

sql_Create_ROCK_OUTCROP <- '
CREATE TABLE ROCK_OUTCROP (
  ID          INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  LANDFORM_ID INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  ABUNDANCE   REAL,
  LITHOLOGY   TEXT
);
'

sql_Create_SEGREGATIONS  <- '
CREATE TABLE SEGREGATIONS (
  ID         INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  HORIZON_ID INTEGER REFERENCES HORIZON (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  ABUNDANCE  REAL,
  SEG_SIZE   REAL,
  NATURE     TEXT,
  FORM       TEXT,
  STRENGTH   INTEGER
);
'

sql_Create_SEGREGATIONS_LANDFORM  <- '
CREATE TABLE SEGREGATIONS_LANDFORM (
  ID          INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  LANDFORM_ID INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  ABUNDANCE   REAL,
  SEG_SIZE    REAL,
  NATURE      TEXT,
  FORM        TEXT,
  STRENGTH    INTEGER
);
'

sql_Create_STRUCTURE  <- '
CREATE TABLE STRUCTURE (
  ID             INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  HORIZON_ID     INTEGER REFERENCES HORIZON (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  PEDALITY_GRADE TEXT,
  FABRIC         TEXT,
  PEDALITY_SIZE  REAL,
  PEDALITY_TYPE  TEXT
);
'

sql_Create_SUBSTRATE  <- '
CREATE TABLE SUBSTRATE (
  ID           INTEGER PRIMARY KEY
  UNIQUE
  NOT NULL,
  LANDFORM_ID  INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  OBSERV_TYPE  TEXT,
  DISTANCE     REAL,
  CONFIDENCE   TEXT,
  DEPTH        REAL,
  STRENGTH     TEXT,
  ALTERATION   TEXT,
  LITHOLOGY    TEXT,
  GENETIC_TYPE TEXT
);
'

sql_Create_SURFACE_CONDITION  <- '
CREATE TABLE SURFACE_CONDITION (
  LANDFORM_ID INTEGER REFERENCES LANDFORM (ID) ON DELETE CASCADE
  ON UPDATE CASCADE,
  CONDITION   TEXT,
  ID          INTEGER
);
'

sql_Create_SURVEY  <- '
CREATE TABLE SURVEY (
  ID          INTEGER PRIMARY KEY
  UNIQUE,
  BUSINESS_ID TEXT,
  NAME        TEXT,
  SURVEY_TYPE TEXT,
  MAP_SCALE   INTEGER,
  VOLUNTEER   INTEGER,
  PUBLIC_DATA TEXT
);
'

# sql_Create_USER_TABLE  <- '
# CREATE TABLE USER_TABLE (
#   ID           INTEGER PRIMARY KEY,
#   USERNAME     TEXT,
#   IS_ADMIN     TEXT,
#   IS_SALI_USER TEXT,
#   IS_VOLUNTEER TEXT,
#   IS_GOVT      TEXT,
#   PASSWORD     TEXT,
#   IS_VIEWER    TEXT,
#   FIRST_NAME   TEXT,
#   LAST_NAME    TEXT,
#   AGENCY       TEXT,
#   IS_ACTIVE    TEXT,
#   ACTIVATED_TS TEXT
# );
# '



