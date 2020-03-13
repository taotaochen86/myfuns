
getDataSet <- function(path) {
  TXTfiles <-
    map(path, dir, full.names = TRUE) %>% map(str_to_upper) %>% flatten_chr()
  dataType <-
    TXTfiles %>% map_chr(str_extract, pattern = "(?<=DAY\\-)[^_-]+(?=\\-)")
  FILES <- tibble(Type = dataType, file = TXTfiles) %>% nest(-Type)

  funlist  <-  {
    list(
      PRS = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1 ,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            PRS_mean = V8,
            #平均温度
            PRS_max = V9,
            #最高气温
            PRS_min = V10 #最低气温
          )
      },
      TEM = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            TEM_mean = V8,
            #平均温度
            TEM_max = V9,
            #最高气温
            TEM_min = V10
          ) #最低气温
      },
      RHU = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            RHU_mean = V8,
            #平均相对湿度
            RHU_min = V9
          ) #最小相对湿度(仅自记)
      },
      PRE = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            PRE20_8 = V8,
            #20-8时降水量
            PRE8_20 = V9,
            #8-20时降水量
            PRE20_20 = V10
          ) #20-20时累计降水量
      },
      EVP = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            EVP_small = V8,
            #小型蒸发量
            EVP_large = V9
          ) #大型蒸发量
      },
      WIN = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            WIN_mean = V8,
            #平均风速
            WIN_max = V9,
            # 最大风速
            WIN_max_d = V10,
            #最大风速的风向
            Win_peak = V11,
            #极大风速
            Win_peak_d = V12 #极大风速
          ) #大型蒸发量
      },
      SSD = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            SSD_mean = V8 #日照时数
          )
      },
      GST = function(x) {
        read.table(
          file = x,
          header = FALSE,
          fileEncoding = "utf-8",
          fill = TRUE
        )  %>%
          select(
            SID = V1,
            #SID
            Y = V5,
            #年
            M = V6,
            #月
            D = V7,
            #日
            GST_mean = V8,
            #平均地表气温
            GST_max = V9,
            #日最高地表气温
            GST_min = V10 #日最低地表气温
          )
      }
    )
  }

  read_table_fromFILEs <- function(type, files) {
    fun <- funlist[[type]]
    files %>% map_df(fun)
  }

  plan(multiprocess, workers = availableCores() - 1)

  map2(FILES$Type,
       FILES$data ,
       ~ future(
         read_table_fromFILEs(.x, .y$file),
         packages = c("purrr", "dplyr")
       )) %>%
    set_names(c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")) %>%
    map(~ value(.x))
}

DS.preprocessed <- function(DS,YearRange) {
  require(glue)
  require(lubridate)
  alldates <-
    lubridate::ymd(glue("{YearRange[1]}-01-01")):lubridate::ymd(glue("{YearRange[2]}-12-31")) %>%
    lubridate::as_date() %>% data.frame(YMD = .) %>%
    separate(col = "YMD", into = c("Y", "M", "D")) %>%
    dplyr::mutate_if(is.character, as.integer)

  alldates <- tibble(SID=DS$PRE$SID %>% unique(),data=list(alldates)) %>% unnest()

  DS$EVP <-
    DS$EVP %>% select(SID, Y,  M , D, EVP_small, EVP_large) %>%
    dplyr::mutate(EVP = if_else(EVP_large == 32766L, as.numeric(EVP_small), EVP_large)) %>%
    select(SID, Y, M, D, EVP) %>%
    dplyr::mutate(EVP = case_when(EVP == 32700 ~ NaN,
                                  EVP == 32733 ~ NaN,
                                  TRUE ~ EVP / 10)) %>%
    unique.data.frame() %>%
    dplyr::left_join(x = alldates,
                     y = .,
                     by = c("SID","Y", "M", "D"))


  DS$GST <- DS$GST %>%
    select(SID, Y,  M , D, GST_mean, GST_max, GST_min) %>%
    dplyr::mutate_at(
      vars(GST_mean, GST_max, GST_min),
      .funs =
        function(x)
          case_when(x == 32700 ~ NaN,
                    x >= 10000 ~ NaN,
                    x <= -10000 ~ NaN,
                    TRUE ~ x / 10)
    )%>%
    unique.data.frame()

  DS$PRE <- DS$PRE  %>%
    select(SID, Y,  M , D, PRE20_20) %>%
    dplyr::mutate_at(
      vars(PRE20_20),
      .funs = function(x) {
        case_when(
          x == 999990 ~ NaN,
          x == 32766 ~ NaN,
          x == NaN ~ NaN,
          x == 32700 ~ 0,
          x >= 32000 ~ (x - 32000) / 10,
          x >= 31000 ~ (x - 31000) / 10,
          x >= 30000 ~ (x - 30000) / 10,
          TRUE ~ x / 10
        )
      }
    )%>%
    unique.data.frame()


  DS$PRS <- DS$PRS %>%
    select(SID, Y,  M , D, PRS_mean, PRS_max, PRS_min) %>%
    dplyr::mutate_at(
      vars(PRS_mean, PRS_max, PRS_min),
      .funs = function(x) {
        case_when(x == 32766 ~ NaN,
                  x == NaN ~ NaN,
                  x >= 20000 ~ (x - 20000) / 10,
                  TRUE ~ x / 10)
      }
    )%>%
    unique.data.frame()


  DS$RHU <- DS$RHU  %>%
    select(SID, Y,  M , D, RHU_mean, RHU_min)  %>%
    dplyr::mutate_at(
      vars(RHU_mean, RHU_min),
      .funs = function(x) {
        case_when(x == 32766 ~ NaN,
                  is.nan(x) ~ NaN,
                  x >= 300 ~ x - 300,
                  TRUE ~ x / 1.0)
      }
    )%>%
    unique.data.frame()

  DS$SSD <- DS$SSD  %>%
    select(SID, Y,  M , D, SSD_mean)  %>%
    dplyr::mutate_at(
      vars(SSD_mean),
      .funs = function(x) {
        case_when(x == 32766 ~ NaN,
                  is.nan(x) ~ NaN,
                  TRUE ~ x / 10)
      }
    )%>%
    unique.data.frame()


  DS$TEM <- DS$TEM %>%
    dplyr::select(SID, Y,  M , D, TEM_mean, TEM_max, TEM_min) %>%
    dplyr::mutate_at(
      vars(TEM_mean, TEM_max, TEM_min),
      .funs = function(x) {
        case_when(x == 32766 ~ NaN,
                  is.nan(x) ~ NaN,
                  TRUE ~ x / 10)
      }
    )%>%
    unique.data.frame()


  DS$WIN <- DS$WIN %>%
    select(SID,
           Y,
           M ,
           D,
           WIN_mean,
           WIN_max,
           WIN_max_d,
           Win_peak,
           Win_peak_d) %>%
    dplyr::mutate_at(
      vars(WIN_mean, WIN_max, Win_peak),
      .funs = function(x) {
        case_when(x == 32766 ~ NaN,
                  is.nan(x) ~ NaN,
                  x >= 1000 ~ (x - 1000) / 10,
                  TRUE ~ x / 10)
      }
    ) %>%  dplyr::mutate_at(
      vars(WIN_max_d, Win_peak_d),
      .funs = function(x) {
        case_when(x == 32766 ~ NaN,
                  is.nan(x) ~ NaN,
                  x >= 17 ~ NaN,
                  TRUE ~ x * 1.0)
      }
    )%>%
    unique.data.frame()

  DS %>% reduce(function(x, y)
    left_join(x, y, by = c("SID", "Y", "M", "D"))) %>%
    select(
      SID,
      Y,
      M,
      D,
      EVP,
      PRE20_20,
      PRS_mean,
      RHU_mean,
      TEM_mean,
      TEM_max,
      TEM_min,
      SSD_mean,
      WIN_mean
    ) %>%
    arrange(SID, Y, M, D)
}

ET0.daily <- function(ds, latitude, altitude){
  ### 构建参数列表---
  constants = list(
    Cp = 1.013e-3,
    #恒压下的比热
    Epsilon = 0.622,
    #水蒸气与干空气分子重量之比
    sigma = 4.9e-9,
    #玻尔兹曼常数
    a = 0.25,
    #太阳辐射回归系数a =0.25  辽宁地区 a=0.18
    b = 0.5,
    #太阳辐射回归系数b=0.5
    alpha = 0.23 #冠层反射系数
  )


  .data <- ds %>% dplyr::mutate(
    Tmax = TEM_max,
    # 最高温度
    Tmm = (TEM_max + TEM_min) / 2,
    # 平均温度
    Tmin = TEM_min,
    # 最小温度
    J = difftime(
      as.Date(sprintf("%d-%02d-%02d", Y, M, D), "%Y-%m-%d"),
      as.Date(sprintf("%d-01-01", Y), "%Y-%m-%d"),
      units = "days"
    ) %>% as.numeric() + 1,
    n = SSD_mean,
    # 日照时数
    uz = WIN_mean,
    # 10m处风速
    RH = RHU_mean,
    #平均相对湿度
    P = PRS_mean,
    #实际大气压
    latitude = latitude,
    altitude = altitude
  ) %>%
    select(Y,
           M,
           D,
           Tmax,
           Tmm,
           Tmin,
           RH,
           n,
           J,
           uz,
           P,
           PRE20_20,
           latitude,
           altitude)



  # lambda 蒸发潜能-----------------------------------------------------------
  cttlambda = 2.501 - 0.002361 * .data$Tmm

  # 气压，若实测存在采用实测，否则采用估算值
  P. = if_else(is.nan(.data$P),
               101.3 * ((293 - 0.0065 * .data$altitude) / 293) ^ 5.26, #估算值
               .data$P / 10) #实测


  # 湿度计常数
  cttgamma = constants$Cp * P. / (constants$Epsilon * cttlambda)

  #### es-ea 饱和水汽压差的计算--------------------------------------------------
  # 饱和水汽压函数
  e0 <- function(Tmm)
    0.6108 * exp(17.27 * Tmm / (Tmm + 237.3))

  # 饱和水汽压
  es <- (e0(.data$Tmax) + e0(.data$Tmin)) / 2

  # 空气实际水汽压
  ea <- es * .data$RH / 100

  #饱和水汽压差
  VPD <- es * (1 - .data$RH / 100)

  #### Rns太阳净短波辐射------------------------------------------------------------

  #日地相对距离
  cttdr = 1 + 0.033 * cos(2 * pi / 365 * .data$J)

  #太阳偏磁角
  cttdelta = 0.409 * sin(2 * pi / 365 * .data$J - 1.39)

  #纬度 弧度制
  cttphi = .data$latitude / 180 * pi

  #日落时角
  cttomega = acos(-tan(cttphi) * tan(cttdelta))

  #理论太阳辐射
  Ra = 24 * 60 / pi * 0.0820 * cttdr * (cttomega * sin(cttphi) * sin(cttdelta) + cos(cttdelta) * sin(cttomega) * cos(cttphi))

  #最大可能日照时数
  cttN = 24 / pi * cttomega

  #相对日照时数
  Rs = (constants$a + constants$b * .data$n / cttN) * Ra

  #太阳净短波辐射（MJ/m^2 d)
  Rns = (1 - constants$alpha) * Rs

  ### Rnl 太阳长短波辐射 --------------------------------------------------------

  # 摄氏度转华氏度
  Tk <- function(T)
    T + 273
  # 填空晴朗时太阳辐射
  Rso = (0.75 + 2e-5 * .data$altitude) * Ra

  ###太阳长短波辐射
  Rnl = constants$sigma * ((Tk(.data$Tmax)) ^ 4 + (Tk(.data$Tmin)) ^ 4) / 2 * (1.35 * Rs / Rso - 0.35) * (0.34 - 0.14 * sqrt(ea))

  ###灌层表面净辐射量
  Rn = Rns - Rnl

  ### 饱和水汽压-温度关系曲线斜率 （kpa/0C)-----
  cttDelta = 4098 * (0.6108 * exp(17.27 * .data$Tmm / (.data$Tmm + 237.3))) / (.data$Tmm + 237.3) ^ 2

  ### G土壤热通量 -----------------------------------------------------------------------
  G = 0

  #逐日ET0的计算可忽略

  ### u2两米处的风速 ----------------------------------------------------------------------
  u2 <- .data$uz * 0.75


  .ET0 <-
    (0.408 * cttDelta * (Rn - G) + cttgamma * 900 / (.data$Tmm + 273) * u2 * VPD) / (cttDelta + cttgamma * (1 + 0.34 * u2))

  # ### 保留变量 Y, M, D, Tmax, T, Tmin, RH, n, U2, VPD, Ra,Rn, ET0
  .data <-
    .data %>% dplyr::mutate(
      Rs = Rs ,
      U2 = u2,
      ET0 = .ET0,
      Rainfall = PRE20_20,
      G = G,
      P = P.
    ) %>%
    select(-J, -uz) %>% dplyr::mutate(Tmean = ds$TEM_mean) %>%
    select(Y,
           Y,
           M,
           D,
           Tmax,
           Tmean,
           Tmeamm = Tmm,
           Tmin,
           RH,
           Rs,
           U2,
           P,
           Rainfall,
           ET0,
           G)
  return(.data)
}

get.StationInfo <- function() {
   load(file = "data/NECStationINFO.Rdata")
   NECStationINFO %>%
    dplyr::select(SID,
                  StationNam,
                  EnStationName = EnStationN,
                  EnProvince,
                  latitude,
                  longitude,
                  altitude) %>%
    dplyr::mutate(altitude = if_else(altitude > 10000, altitude - 10000, altitude))
}
