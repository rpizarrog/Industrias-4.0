# Reglas de modelo de regesion
# para datos de NY
# Función basada en la HERRAMIENTA BigML para la generación de árbol de Regresi+on con datos de NY
predecirTemperatura <- function(ozono=NA,
                               radiacion=NA,
                               vel_viento=NA,
                               mes=NA){
  if (is.na(mes)){
    return(77.88235)
  }
  if (mes > 5) {
    if (is.na(ozono)){
      return(81.01639)
    }
    if (ozono > 30) {
      if (mes > 6) {
        if (ozono > 69) {
          if (ozono > 128) {
            if (is.na(vel_viento)){
              return(83.23077)
            }
            if (vel_viento > 140) {
              return(91)
            }
            if (vel_viento <= 140) {
              if (vel_viento > 120) {
                if (vel_viento > 129) {
                  return(77)
                }
                if (vel_viento <= 129) {
                  return(75)
                }
              }
              if (vel_viento <= 120) {
                if (is.na(radiacion)){
                  return(83.9)
                }
                if (radiacion > 82) {
                  if (radiacion > 230) {
                    if (ozono > 151) {
                      if (vel_viento > 106) {
                        return(82)
                      }
                      if (vel_viento <= 106) {
                        return(81)
                      }
                    }
                    if (ozono <= 151) {
                      return(84)
                    }
                  }
                  if (radiacion <= 230) {
                    if (radiacion > 146) {
                      if (vel_viento > 71) {
                        return(92)
                      }
                      if (vel_viento <= 71) {
                        return(88)
                      }
                    }
                    if (radiacion <= 146) {
                      if (vel_viento > 97) {
                        if (vel_viento > 112) {
                          return(86)
                        }
                        if (vel_viento <= 112) {
                          return(84)
                        }
                      }
                      if (vel_viento <= 97) {
                        return(82)
                      }
                    }
                  }
                }
                if (radiacion <= 82) {
                  return(79)
                }
              }
            }
          }
          if (ozono <= 128) {
            if (mes > 7) {
              if (is.na(vel_viento)){
                return(91.61538)
              }
              if (vel_viento > 15) {
                if (vel_viento > 66) {
                  if (ozono > 77) {
                    if (ozono > 83) {
                      if (vel_viento > 86) {
                        return(90)
                      }
                      if (vel_viento <= 86) {
                        return(91)
                      }
                    }
                    if (ozono <= 83) {
                      return(86)
                    }
                  }
                  if (ozono <= 77) {
                    return(97)
                  }
                }
                if (vel_viento <= 66) {
                  if (is.na(radiacion)){
                    return(93.66667)
                  }
                  if (radiacion > 231) {
                    return(96)
                  }
                  if (radiacion <= 231) {
                    if (mes > 8) {
                      if (vel_viento > 48) {
                        return(92)
                      }
                      if (vel_viento <= 48) {
                        return(93)
                      }
                    }
                    if (mes <= 8) {
                      return(94)
                    }
                  }
                }
              }
              if (vel_viento <= 15) {
                if (ozono > 91) {
                  if (vel_viento > 6) {
                    return(90)
                  }
                  if (vel_viento <= 6) {
                    return(89)
                  }
                }
                if (ozono <= 91) {
                  return(86)
                }
              }
            }
            if (mes <= 7) {
              if (is.na(vel_viento)){
                return(88.375)
              }
              if (vel_viento > 54) {
                if (vel_viento > 68) {
                  if (vel_viento > 80) {
                    return(86)
                  }
                  if (vel_viento <= 80) {
                    if (is.na(radiacion)){
                      return(88.5)
                    }
                    if (radiacion > 194) {
                      return(88)
                    }
                    if (radiacion <= 194) {
                      return(89)
                    }
                  }
                }
                if (vel_viento <= 68) {
                  return(92)
                }
              }
              if (vel_viento <= 54) {
                if (vel_viento > 29) {
                  if (is.na(radiacion)){
                    return(87.5)
                  }
                  if (radiacion > 231) {
                    return(88)
                  }
                  if (radiacion <= 231) {
                    return(87)
                  }
                }
                if (vel_viento <= 29) {
                  return(85)
                }
              }
            }
          }
        }
        if (ozono <= 69) {
          if (ozono > 46) {
            if (is.na(radiacion)){
              return(83.61538)
            }
            if (radiacion > 66) {
              if (radiacion > 126) {
                if (ozono > 65) {
                  return(87)
                }
                if (ozono <= 65) {
                  if (radiacion > 166) {
                    if (ozono > 48) {
                      if (ozono > 54) {
                        if (ozono > 60) {
                          if (ozono > 63) {
                            return(83)
                          }
                          if (ozono <= 63) {
                            if (is.na(vel_viento)){
                              return(84.5)
                            }
                            if (vel_viento > 89) {
                              return(85)
                            }
                            if (vel_viento <= 89) {
                              return(84)
                            }
                          }
                        }
                        if (ozono <= 60) {
                          return(81)
                        }
                      }
                      if (ozono <= 54) {
                        if (is.na(vel_viento)){
                          return(85.5)
                        }
                        if (vel_viento > 83) {
                          return(85)
                        }
                        if (vel_viento <= 83) {
                          return(86)
                        }
                      }
                    }
                    if (ozono <= 48) {
                      return(81)
                    }
                  }
                  if (radiacion <= 166) {
                    return(80)
                  }
                }
              }
              if (radiacion <= 126) {
                if (is.na(vel_viento)){
                  return(86.5)
                }
                if (vel_viento > 43) {
                  return(87)
                }
                if (vel_viento <= 43) {
                  return(86)
                }
              }
            }
            if (radiacion <= 66) {
              return(79)
            }
          }
          if (ozono <= 46) {
            if (is.na(vel_viento)){
              return(81.30769)
            }
            if (vel_viento > 112) {
              if (is.na(radiacion)){
                return(83.66667)
              }
              if (radiacion > 214) {
                return(81)
              }
              if (radiacion <= 214) {
                if (vel_viento > 135) {
                  return(84)
                }
                if (vel_viento <= 135) {
                  return(86)
                }
              }
            }
            if (vel_viento <= 112) {
              if (ozono > 42) {
                if (vel_viento > 83) {
                  if (vel_viento > 100) {
                    return(78)
                  }
                  if (vel_viento <= 100) {
                    return(79)
                  }
                }
                if (vel_viento <= 83) {
                  return(78)
                }
              }
              if (ozono <= 42) {
                if (ozono > 31) {
                  if (is.na(radiacion)){
                    return(82.16667)
                  }
                  if (radiacion > 172) {
                    if (vel_viento > 83) {
                      if (vel_viento > 97) {
                        if (vel_viento > 106) {
                          return(83)
                        }
                        if (vel_viento <= 106) {
                          return(82)
                        }
                      }
                      if (vel_viento <= 97) {
                        return(81)
                      }
                    }
                    if (vel_viento <= 83) {
                      return(85)
                    }
                  }
                  if (radiacion <= 172) {
                    return(81)
                  }
                }
                if (ozono <= 31) {
                  return(78)
                }
              }
            }
          }
        }
      }
      if (mes <= 6) {
        if (is.na(vel_viento)){
          return(79.91667)
        }
        if (vel_viento > 155) {
          if (vel_viento > 184) {
            return(72)
          }
          if (vel_viento <= 184) {
            return(67)
          }
        }
        if (vel_viento <= 155) {
          if (is.na(radiacion)){
            return(80.86364)
          }
          if (radiacion > 168) {
            if (ozono > 193) {
              if (radiacion > 279) {
                if (vel_viento > 106) {
                  if (vel_viento > 126) {
                    return(80)
                  }
                  if (vel_viento <= 126) {
                    return(79)
                  }
                }
                if (vel_viento <= 106) {
                  if (vel_viento > 91) {
                    return(74)
                  }
                  if (vel_viento <= 91) {
                    return(78)
                  }
                }
              }
              if (radiacion <= 279) {
                if (vel_viento > 66) {
                  if (vel_viento > 126) {
                    return(79)
                  }
                  if (vel_viento <= 126) {
                    if (radiacion > 235) {
                      if (vel_viento > 80) {
                        if (vel_viento > 100) {
                          return(93)
                        }
                        if (vel_viento <= 100) {
                          return(92)
                        }
                      }
                      if (vel_viento <= 80) {
                        return(87)
                      }
                    }
                    if (radiacion <= 235) {
                      if (vel_viento > 89) {
                        return(84)
                      }
                      if (vel_viento <= 89) {
                        return(85)
                      }
                    }
                  }
                }
                if (vel_viento <= 66) {
                  return(76)
                }
              }
            }
            if (ozono <= 193) {
              if (vel_viento > 126) {
                return(90)
              }
              if (vel_viento <= 126) {
                return(87)
              }
            }
          }
          if (radiacion <= 168) {
            if (radiacion > 94) {
              if (radiacion > 136) {
                if (vel_viento > 35) {
                  return(77)
                }
                if (vel_viento <= 35) {
                  return(83)
                }
              }
              if (radiacion <= 136) {
                if (radiacion > 131) {
                  return(75)
                }
                if (radiacion <= 131) {
                  if (vel_viento > 61) {
                    return(80)
                  }
                  if (vel_viento <= 61) {
                    return(78)
                  }
                }
              }
            }
            if (radiacion <= 94) {
              if (vel_viento > 126) {
                return(77)
              }
              if (vel_viento <= 126) {
                if (vel_viento > 74) {
                  return(73)
                }
                if (vel_viento <= 74) {
                  return(76)
                }
              }
            }
          }
        }
      }
    }
    if (ozono <= 30) {
      if (mes > 8) {
        if (is.na(vel_viento)){
          return(72.5)
        }
        if (vel_viento > 112) {
          if (ozono > 20) {
            return(76)
          }
          if (ozono <= 20) {
            if (vel_viento > 154) {
              return(63)
            }
            if (vel_viento <= 154) {
              if (is.na(radiacion)){
                return(69)
              }
              if (radiacion > 207) {
                if (radiacion > 231) {
                  return(64)
                }
                if (radiacion <= 231) {
                  if (vel_viento > 126) {
                    return(67)
                  }
                  if (vel_viento <= 126) {
                    return(68)
                  }
                }
              }
              if (radiacion <= 207) {
                if (vel_viento > 129) {
                  return(75)
                }
                if (vel_viento <= 129) {
                  return(71)
                }
              }
            }
          }
        }
        if (vel_viento <= 112) {
          if (vel_viento > 66) {
            if (ozono > 23) {
              if (is.na(radiacion)){
                return(70.33333)
              }
              if (radiacion > 248) {
                return(73)
              }
              if (radiacion <= 248) {
                if (vel_viento > 86) {
                  return(68)
                }
                if (vel_viento <= 86) {
                  return(70)
                }
              }
            }
            if (ozono <= 23) {
              if (is.na(radiacion)){
                return(74.28571)
              }
              if (radiacion > 134) {
                if (radiacion > 241) {
                  return(80)
                }
                if (radiacion <= 241) {
                  if (vel_viento > 106) {
                    return(75)
                  }
                  if (vel_viento <= 106) {
                    return(78)
                  }
                }
              }
              if (radiacion <= 134) {
                if (ozono > 11) {
                  if (vel_viento > 97) {
                    return(76)
                  }
                  if (vel_viento <= 97) {
                    return(71)
                  }
                }
                if (ozono <= 11) {
                  if (vel_viento > 106) {
                    return(71)
                  }
                  if (vel_viento <= 106) {
                    return(69)
                  }
                }
              }
            }
          }
          if (vel_viento <= 66) {
            if (ozono > 17) {
              if (vel_viento > 35) {
                return(77)
              }
              if (vel_viento <= 35) {
                return(76)
              }
            }
            if (ozono <= 17) {
              return(82)
            }
          }
        }
      }
      if (mes <= 8) {
        if (is.na(radiacion)){
          return(77.33333)
        }
        if (radiacion > 42) {
          if (ozono > 14) {
            if (ozono > 25) {
              if (is.na(vel_viento)){
                return(81.66667)
              }
              if (vel_viento > 132) {
                return(81)
              }
              if (vel_viento <= 132) {
                return(82)
              }
            }
            if (ozono <= 25) {
              if (ozono > 20) {
                if (is.na(vel_viento)){
                  return(77.8)
                }
                if (vel_viento > 41) {
                  if (vel_viento > 88) {
                    return(77)
                  }
                  if (vel_viento <= 88) {
                    return(76)
                  }
                }
                if (vel_viento <= 41) {
                  return(82)
                }
              }
              if (ozono <= 20) {
                return(82)
              }
            }
          }
          if (ozono <= 14) {
            if (radiacion > 84) {
              if (is.na(vel_viento)){
                return(74)
              }
              if (vel_viento > 109) {
                return(73)
              }
              if (vel_viento <= 109) {
                return(76)
              }
            }
            if (radiacion <= 84) {
              return(80)
            }
          }
        }
        if (radiacion <= 42) {
          if (mes > 6) {
            if (is.na(vel_viento)){
              return(75.66667)
            }
            if (vel_viento > 140) {
              return(72)
            }
            if (vel_viento <= 140) {
              if (vel_viento > 103) {
                return(81)
              }
              if (vel_viento <= 103) {
                return(74)
              }
            }
          }
          if (mes <= 6) {
            return(65)
          }
        }
      }
    }
  }
  if (mes <= 5) {
    if (is.na(radiacion)){
      return(65.54839)
    }
    if (radiacion > 108) {
      if (is.na(ozono)){
        return(68.09524)
      }
      if (ozono > 215) {
        if (radiacion > 199) {
          if (is.na(vel_viento)){
            return(57)
          }
          if (vel_viento > 146) {
            return(58)
          }
          if (vel_viento <= 146) {
            if (vel_viento > 75) {
              return(56)
            }
            if (vel_viento <= 75) {
              return(57)
            }
          }
        }
        if (radiacion <= 199) {
          return(69)
        }
      }
      if (ozono <= 215) {
        if (ozono > 43) {
          if (is.na(vel_viento)){
            return(80)
          }
          if (vel_viento > 103) {
            return(81)
          }
          if (vel_viento <= 103) {
            return(79)
          }
        }
        if (ozono <= 43) {
          if (radiacion > 284) {
            if (is.na(vel_viento)){
              return(66.28571)
            }
            if (vel_viento > 140) {
              return(73)
            }
            if (vel_viento <= 140) {
              if (ozono > 26) {
                if (vel_viento > 63) {
                  return(68)
                }
                if (vel_viento <= 63) {
                  return(66)
                }
              }
              if (ozono <= 26) {
                if (vel_viento > 103) {
                  if (radiacion > 323) {
                    return(64)
                  }
                  if (radiacion <= 323) {
                    return(62)
                  }
                }
                if (vel_viento <= 103) {
                  if (vel_viento > 89) {
                    return(66)
                  }
                  if (vel_viento <= 89) {
                    return(65)
                  }
                }
              }
            }
          }
          if (radiacion <= 284) {
            if (radiacion > 276) {
              return(76)
            }
            if (radiacion <= 276) {
              if (ozono > 13) {
                if (is.na(vel_viento)){
                  return(68.4)
                }
                if (vel_viento > 41) {
                  if (radiacion > 230) {
                    if (vel_viento > 103) {
                      return(68)
                    }
                    if (vel_viento <= 103) {
                      return(69)
                    }
                  }
                  if (radiacion <= 230) {
                    if (vel_viento > 111) {
                      return(66)
                    }
                    if (vel_viento <= 111) {
                      return(67)
                    }
                  }
                }
                if (vel_viento <= 41) {
                  return(72)
                }
              }
              if (ozono <= 13) {
                return(74)
              }
            }
          }
        }
      }
    }
    if (radiacion <= 108) {
      if (is.na(vel_viento)){
        return(60.2)
      }
      if (vel_viento > 54) {
        if (radiacion > 54) {
          if (vel_viento > 152) {
            return(57)
          }
          if (vel_viento <= 152) {
            if (vel_viento > 135) {
              return(59)
            }
            if (vel_viento <= 135) {
              return(58)
            }
          }
        }
        if (radiacion <= 54) {
          if (radiacion > 13) {
            if (radiacion > 34) {
              return(62)
            }
            if (radiacion <= 34) {
              return(61)
            }
          }
          if (radiacion <= 13) {
            return(59)
          }
        }
      }
      if (vel_viento <= 54) {
        if (radiacion > 52) {
          return(61)
        }
        if (radiacion <= 52) {
          return(67)
        }
      }
    }
  }
  return(NA)
}
