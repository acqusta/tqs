package xtz.tquant.stra.utils

import java.time.{LocalDate, LocalTime}

object TimeUtils {

    implicit class LocalTimeConvertor(time: LocalTime) {
        def toHumanMilli = {
            (time.getHour * 10000 + time.getMinute * 100 + time.getSecond) * 1000 + time.getNano / 1000000
        }

        def toHumanSecond = {
            time.getHour * 10000 + time.getMinute * 100 + time.getSecond
        }
    }

    implicit class LocalDateConvertor(date: LocalDate) {
        def toHumanDay =
            date.getYear * 10000 + date.getMonthValue * 100 + date.getDayOfMonth

    }
}
