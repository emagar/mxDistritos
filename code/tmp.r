##############################################
## CREATION OF NEW MUNICIPIOS IN THE PERIOD ##
##############################################
## In order to prevent NAs when estimating expected vote with five previous races, this code block
## splits and aggregates secciones belonging to new/old municipalities. In many cases the municipal
## split went to court. IFE/INE are conservative, only incorporating new units after courts have
## ruled. Others usually do this before courts rule: local authorities (prior to 2016) elected municipal
## authorities, INEGI incorporates them in census/counts. These manipulations will use the less conservative
## criterion (in order to analyze municipal races), splitting federal election returns earlier than IFE/INE
## would.

1. [DONE] in eq, subset children and parents
2. [DONE] merge mun94 into v94, mun97 into v97...
3. [DONE] agg municpios: v94m... (non manip)
4. [DONE] agg municpios: v94manip...
5. consolidate square vmanip matrix 
6. list all secciones needing new mun manip in any year 1994:2018 (so that v..manip all have same nrows)
7. fix/save mun data
8. regress vhat for non-manip and manip data
9. using yrchg, identify 5 obs that need replacement
10. save m regs

   
#######################

# estimate regs for manipulated municipal aggregates
if (agg=="m") {
    v91 <- v91m; # change with v91manip when 1991 seccion-level data available
    v94 <- v94manip; v97 <- v97manip; 
    v00 <- v00manip; v03 <- v03manip; v06 <- v06manip; v09 <- v09manip; v12 <- v12manip; v15 <- v15manip; v18 <- v18manip;
}
###########################################
## prepare manipulated party objects     ##
## for time-series and alpha regressions ##
###########################################
#
# version 1: extend partial coalitions across the board
# shares
pan <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pan  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pan  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pan  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$panc / v00$efec),
                  v03 = ifelse(v03$efec==0, NA,  v03$pan  / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pan  / v06$efec),
                  v09 = ifelse(v09$efec==0, NA,  v09$pan  / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pan  / v12$efec),
                  v15 = ifelse(v15$efec==0, NA,  v15$pan  / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pan + v18$panc + v18$prd + v18$mc) / v18$efec))
pan <- round(pan, 3)
#
pri <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pri  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pri  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pri  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$pri / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$pri + v03$pric + v03$pvem) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pric / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pri + v09$pric + v09$pvem) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA, (v12$pri + v12$pric + v12$pvem) / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$pri + v15$pric + v15$pvem) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pri + v18$pric + v18$pvem + v18$pna) / v18$efec))
pri <- round(pri, 3)
#
left <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$prd  / v91$efec),
                     v94 = ifelse(v94$efec==0, NA,  v94$prd  / v94$efec),
                     v97 = ifelse(v97$efec==0, NA,  v97$prd  / v97$efec),
                     v00 = ifelse(v00$efec==0, NA,  v00$prdc / v00$efec),
                     v03 = ifelse(v03$efec==0, NA, (v03$prd + v03$pt + v03$conve) / v03$efec),
                     v06 = ifelse(v06$efec==0, NA,  v06$prdc / v06$efec),
                     v09 = ifelse(v09$efec==0, NA, (v09$prd + v09$pt + v09$ptc + v09$conve) / v09$efec),
                     v12 = ifelse(v12$efec==0, NA, (v12$prd + v12$prdc + v12$pt + v12$mc)  / v12$efec),
                     v15 = ifelse(v15$efec==0, NA, (v15$prd + v15$prdc + v15$pt + v15$morena + v15$pes) / v15$efec),
                     v18 = ifelse(v18$efec==0, NA, (v18$morena + v18$morenac + v18$pt + v18$pes) / v18$efec))
left <- round(left, 3)
#
oth <- data.frame(v91 = ifelse(v91$efec==0, NA, (v91$parm + v91$pdm + v91$pfcrn + v91$pps + v91$pem + v91$prt) / v91$efec),
                  v94 = ifelse(v94$efec==0, NA, (v94$pps + v94$pfcrn + v94$parm + v94$uno.pdm + v94$pt + v94$pvem) / v94$efec),
                  v97 = ifelse(v97$efec==0, NA, (v97$pc + v97$pt + v97$pvem + v97$pps + v97$pdm) / v97$efec),
                  v00 = ifelse(v00$efec==0, NA, (v00$pcd + v00$parm + v00$dsppn) / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$psn + v03$pas + v03$mp + v03$plm + v03$fc) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA, (v06$pna + v06$asdc) / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pna + v09$psd) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pna / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$mc + v15$pna + v15$ph + v15$indep1 + v15$indep2) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$indep1 + v18$indep2) / v18$efec))
oth <- round(oth, 3)
#
efec <- data.frame(v91 = v91$efec,
                   v94 = v94$efec,
                   v97 = v97$efec,
                   v00 = v00$efec,
                   v03 = v03$efec,
                   v06 = v06$efec,
                   v09 = v09$efec,
                   v12 = v12$efec,
                   v15 = v15$efec,
                   v18 = v18$efec)

#
# transpose to plug columns into new data.frames
pan <- t(pan)
pri <- t(pri)
left <- t(left)
oth <- t(oth)
efec <- t(efec)
#
extendCoal <- as.list(rep(NA, nrow(v00))) # empty list will receive one data.frame per municipio
# loop over municipios/secciones
for (i in 1:nrow(v00)){
    #i <- 81 # debug
    tmp <- data.frame(yr = seq(from=1991, to=2018, by=3),
                      pan = pan[,i],
                      pri = pri[,i],
                      left = left[,i],
                      oth = oth[,i],
                      efec = efec[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
    }
    # add epsilon = 2*max(rounding error) to zeroes 
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # fill info to new list
    extendCoal[[i]] <- tmp
}
# datos para regresión de alfa
# son los mismos que para las regresiones con v..m
# [... skipped many lines of code here...]
# plug into data
for (i in 1:nrow(v00)){
    #i <- 2 # debug
    extendCoal[[i]] <- cbind(extendCoal[[i]], yr.means[,6:8])
}

######################


ls()
tmp <- eq.new[,  c("edosecn","dmunchg","mun1994")] # subset 1994 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v94manip <- merge(x = v94, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
v94manip[1,]





x

table(treat.yrs$yr.chg, useNA = "always")
x
################

dim(eq.new)
v94manip[1,]
length(sel.r)
dim(v18)
x

# PLAYAS DE ROSARITO
cycle.chg <- 10
cycle.first <- cycle.chg - 5 
cycle.last  <- cycle.chg + 5
cycles.manip <- seq(cycle.first, cycle.last, by = 1)

child <- c(1194, 1259, 1260, 1261, 1262, 1263, 1264, 1265, 1266, 1267, 1268, 1269, 1270, 1271, 1272, 1273, 1290, 1291, 1295, 1297, 1298, 1363, 1364, 1365, 1366, 1367, 1369, 1370)
parent.new <- c(
735, 736, 737, 738, 739, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749, 750, 751, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761, 762, 763, 764,
765, 766, 767, 768, 769, 770, 771, 772, 773, 774, 775, 776, 777, 778, 779, 780, 781, 782, 783, 784, 785, 786, 787, 788, 789, 790, 791, 792, 793, 794,
795, 796, 797, 798, 799, 800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 823, 824,
825, 826, 827, 828, 829, 830, 831, 832, 833, 834, 835, 836, 837, 838, 839, 840, 841, 842, 843, 844, 845, 846, 847, 848, 849, 850, 851, 852, 853, 854,
855, 856, 857, 858, 859, 860, 861, 862, 863, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 877, 878, 879, 880, 881, 882, 883, 884,
885, 886, 887, 888, 889, 890, 891, 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914,
915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927, 928, 929, 930, 931, 932, 933, 934, 935, 936, 937, 938, 939, 940, 941, 942, 943, 944,
945, 946, 947, 948, 949, 950, 951, 952, 953, 954, 955, 956, 957, 958, 959, 960, 961, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974,
975, 976, 977, 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 989, 990, 991, 992, 993, 994, 995, 996, 997, 998, 999, 1000, 1001, 1002, 1003,
1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020, 1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028,
1029, 1030, 1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 1040, 1041, 1042, 1043, 1044, 1045, 1046, 1047, 1048, 1049, 1050, 1051, 1052, 1053,
1054, 1055, 1056, 1057, 1058, 1059, 1060, 1061, 1062, 1063, 1064, 1065, 1066, 1067, 1068, 1069, 1070, 1071, 1072, 1073, 1074, 1075, 1076, 1077, 1078,
1079, 1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 1090, 1091, 1092, 1093, 1094, 1095, 1096, 1097, 1098, 1099, 1100, 1101, 1102, 1103,
1104, 1105, 1106, 1107, 1108, 1109, 1110, 1111, 1112, 1113, 1114, 1115, 1116, 1117, 1118, 1119, 1120, 1121, 1122, 1123, 1124, 1125, 1126, 1127, 1128,
1129, 1130, 1131, 1132, 1133, 1134, 1135, 1136, 1137, 1138, 1139, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1151, 1152, 1153,
1154, 1155, 1156, 1157, 1158, 1159, 1160, 1161, 1162, 1163, 1164, 1165, 1166, 1167, 1168, 1169, 1170, 1171, 1172, 1173, 1174, 1175, 1176, 1177, 1178,
1179, 1180, 1181, 1182, 1183, 1184, 1185, 1186, 1187, 1188, 1189, 1190, 1191, 1192, 1193, 1195, 1196, 1197, 1198, 1199, 1200, 1201, 1202, 1203, 1204,
1205, 1206, 1207, 1208, 1209, 1210, 1211, 1212, 1213, 1214, 1215, 1216, 1217, 1218, 1219, 1220, 1221, 1222, 1223, 1224, 1225, 1226, 1227, 1228, 1229,
1230, 1231, 1232, 1233, 1234, 1235, 1236, 1237, 1238, 1239, 1240, 1241, 1242, 1243, 1244, 1245, 1246, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254,
1255, 1256, 1257, 1258, 1274, 1275, 1276, 1277, 1278, 1279, 1280, 1281, 1282, 1283, 1284, 1285, 1286, 1287, 1288, 1289, 1292, 1293, 1294, 1296, 1299,
1300, 1301, 1302, 1303, 1304, 1305, 1306, 1307, 1308, 1309, 1310, 1311, 1312, 1313, 1314, 1315, 1316, 1317, 1318, 1319, 1320, 1321, 1322, 1323, 1324,
1325, 1326, 1327, 1328, 1329, 1330, 1331, 1332, 1333, 1334, 1335, 1336, 1337, 1338, 1339, 1340, 1341, 1342, 1343, 1344, 1345, 1346, 1347, 1348, 1349,
1350, 1351, 1352, 1353, 1354, 1355, 1356, 1357, 1358, 1359, 1360, 1361, 1362, 1368, 1371, 1372, 1373, 1374, 1375, 1376, 1377, 1378, 1379, 1380, 1381,
1382, 1383, 1384, 1385, 1386, 1387, 1388, 1389, 1390, 1391, 1392, 1393, 1394, 1395, 1396, 1397, 1398, 1399, 1400, 1401, 1402, 1403, 1404, 1405, 1406,
1407, 1408, 1409, 1410, 1411, 1412, 1413, 1414, 1415, 1416, 1417, 1418, 1419, 1420, 1421, 1422, 1423, 1424, 1425, 1426, 1427, 1428, 1429, 1430, 1431,
1432, 1433, 1434, 1435, 1436, 1437, 1438, 1439, 1440, 1441, 1442, 1443, 1444, 1445, 1446, 1447, 1448, 1449, 1450, 1451, 1452, 1453, 1454, 1455, 1456,
1457, 1458, 1459, 1460, 1461, 1462, 1463, 1464, 1465, 1466, 1467, 1468, 1469, 1470, 1471, 1472, 1473, 1474, 1475, 1476, 1477, 1478, 1479, 1480, 1481,
1482, 1483, 1484, 1485, 1486, 1487, 1488, 1489, 1490, 1491, 1492, 1493, 1494, 1495, 1496, 1497, 1498, 1499, 1500, 1501, 1502, 1503, 1504, 1505, 1506,
1507, 1508, 1509, 1510, 1511, 1512, 1513, 1514, 1515, 1516, 1517, 1518, 1519, 1520, 1521, 1522, 1523, 1524, 1525, 1526, 1527, 1528, 1529, 1530, 1531,
1532, 1533, 1534, 1535, 1536, 1537, 1538, 1539, 1540, 1541, 1542, 1543, 1544, 1545, 1546, 1547, 1548, 1549, 1550, 1551, 1552, 1553, 1554, 1555, 1556,
1557, 1558, 1559, 1560, 1561, 1562, 1563, 1564, 1565, 1566, 1567, 1568, 1569, 1570, 1571, 1572, 1573, 1574, 1575, 1576, 1577, 1578, 1579, 1580, 1581,
1582, 1583, 1584, 1585, 1586, 1587, 1588, 1589, 1590, 1591, 1592, 1593, 1594, 1595, 1596, 1597, 1598, 1599, 1600, 1601, 1602, 1603, 1604, 1605, 1606,
1607, 1608, 1609, 1610, 1611, 1612, 1613, 1614, 1615, 1616, 1617, 1618, 1619, 1620, 1621, 1622, 1623, 1624, 1625, 1626, 1627, 1628, 1629, 1630, 1631,
1632, 1633, 1634, 1635, 1636, 1637, 1638, 1639, 1640, 1641, 1678, 1679, 1680, 1681, 1682, 1683, 1684, 1685, 1686, 1687, 1688, 1689, 1690, 1691, 1692,
1693, 1694, 1695, 1696, 1697, 1698, 1699, 1700, 1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1713, 1714, 1715, 1716, 1717,
1718, 1719, 1720, 1721, 1722, 1723, 1724, 1725, 1726, 1727, 1728, 1729, 1730, 1731, 1732, 1733, 1734, 1735, 1736, 1737, 1738, 1739, 1740, 1741, 1742,
1743, 1744, 1745, 1746, 1747, 1748, 1749, 1750, 1751, 1752, 1753, 1754, 1755, 1756, 1757, 1758, 1759, 1760, 1761, 1762, 1763, 1764, 1765, 1766, 1767,
1768, 1769, 1770, 1771, 1772, 1773, 1774, 1775, 1776, 1777, 1778, 1779, 1780, 1781, 1782, 1783, 1784, 1785, 1786, 1787, 1788, 1789, 1790, 1791, 1792,
1793, 1794, 1795, 1796, 1797, 1798, 1799, 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1868, 1869, 1870, 1871, 1872, 1873, 1874, 1875, 1876, 1877, 1878,
1879, 1880, 1881, 1882, 1883, 1884, 1885, 1886, 1887, 1888, 1889, 1890, 1891, 1892, 1893, 1894, 1895, 1896, 1897, 1898, 1899, 1900, 1901, 1902, 1903,
1904, 1905, 1906, 1907, 1908, 1909, 1910, 1911, 1912, 1913, 1914, 1915, 1916, 1917, 1918, 1919, 1920, 1921, 1922, 1923, 1924, 1925, 1926, 1927, 1928,
1929, 1930, 1931, 1932, 1933, 1934, 1935, 1936, 1937, 1938, 1939, 1940, 1941, 1942)
parent.old <- c(child, parent.new)

