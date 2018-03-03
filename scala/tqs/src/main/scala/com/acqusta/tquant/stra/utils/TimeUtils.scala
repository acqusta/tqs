package com.acqusta.tquant.stra.utils

import java.time.{LocalDate, LocalDateTime, LocalTime}

object TimeUtils {

    implicit class LocalDateTimeConvertor(dt: LocalDateTime) {
        def toHumanDateTime : (Int, Int) = (toHumanDate, toHumanMilli)

        def toHumanMilli : Int = {
            (dt.getHour * 10000 + dt.getMinute * 100 + dt.getSecond) * 1000 + dt.getNano / 1000000
        }

        def toHumanSecond : Int = {
            dt.getHour * 10000 + dt.getMinute * 100 + dt.getSecond
        }

        def toHumanDate : Int =
            dt.getYear * 10000 + dt.getMonthValue * 100 + dt.getDayOfMonth
    }

    implicit class LocalTimeConvertor(time: LocalTime) {
        def toHumanMilli : Int = {
            (time.getHour * 10000 + time.getMinute * 100 + time.getSecond) * 1000 + time.getNano / 1000000
        }

        def toHumanSecond : Int = {
            time.getHour * 10000 + time.getMinute * 100 + time.getSecond
        }
    }

    implicit class LocalDateConvertor(date: LocalDate) {
        def toHumanDate : Int =
            date.getYear * 10000 + date.getMonthValue * 100 + date.getDayOfMonth

    }

    implicit class IntDateConvertor(dt: Int ) {
        def toLocalDate : LocalDate = LocalDate.of( dt / 10000, (dt%10000)/100, (dt%100))
    }

    implicit class IntTimeConvertor(dt: Int ) {
        def toLocalTime : LocalTime =
            LocalTime.of(dt / 10000000,
                (dt%10000000)/100000,
                (dt%100000)/1000,
                (dt%1000) * 1000000)
    }


    implicit class IntDateTimeConvertor(dt: (Int, Int) ) {

        def toLocalDateTime : LocalDateTime =
            LocalDateTime.of( dt._1.toLocalDate, dt._2.toLocalTime)
    }

}
