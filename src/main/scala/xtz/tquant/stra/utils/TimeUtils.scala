package xtz.tquant.stra.utils

import java.time.{LocalDate, LocalTime}

object TimeUtils {

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

    implicit class IntDateTimeConvertor(dt: Int ) {
        def toLocalDate : LocalDate = LocalDate.of( dt / 10000, (dt%10000)/100, (dt%100))

        def toLocalTime : LocalTime =
            LocalTime.of(dt / 10000000,
                         (dt%10000000)/100000,
                         (dt%100000)/1000,
                         (dt%1000) * 1000000)
    }
}
