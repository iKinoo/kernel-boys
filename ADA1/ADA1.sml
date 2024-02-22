(*
ADA1
Wilbert Díaz Gómez
Rodrigo Joaquín Pacab Canúl
Orlando Isaías Rodríguez Couoh
*)

type fecha = {anio:int, dia:int, mes:int};

fun is_older(fecha1: fecha, fecha2: fecha) =
    let
        val anio1 = #anio fecha1
        val anio2 = #anio fecha2
        val mes1 = #mes fecha1
        val mes2 = #mes fecha2
        val dia1 = #dia fecha1
        val dia2 = #dia fecha2
    in
        if anio1 < anio2 then
            print "Fecha 1 es anterior a Fecha 2 "
        else if anio1 > anio2 then
            print "Fecha 1 es posterior a Fecha 2 "
        else if mes1 < mes2 then
            print "Fecha 1 es anterior a Fecha 2 "
        else if mes1 > mes2 then
            print "Fecha 1 es posterior a Fecha 2 "
        else if dia1 < dia2 then
            print  "Fecha 1 es anterior a Fecha 2 "
        else if dia1 > dia2 then
            print "Fecha 1 es posterior a Fecha 2 "
        else
            print "Ambas fechas son iguales "
    end;

fun number_in_month (datelist : fecha list, month : int) =
    let
    fun count (x : int , datelist : fecha list) =
        if null (tl datelist)
        then if #mes(hd datelist) <> month then x else x+1
        else if #mes(hd datelist) = month
        then count (x+1, tl datelist)
        else count (x, tl datelist)
    in
    count (0, datelist)
    end

fun number_in_months (datelist : fecha list, monthlist : int list) =
    let
        fun count (x : int, monthlist : int list) =
            if null monthlist
            then x
            else count (x + number_in_month (datelist, hd monthlist), tl monthlist)
    in
        count (0, monthlist)
    end;

fun date_to_string(date:fecha) =
    let
        val anio = #anio date
        val mes = #mes date
        val dia = #dia date
        val months = ["", "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"];
        val month_name = List.nth(months, mes);
    in
        month_name ^ " " ^ Int.toString dia ^ ", " ^ Int.toString anio
    end;



fun daysInMonth n =  List.nth ( [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], n-1)

fun isBiciesto y =
    if y mod 400 = 0 then true
    else if y mod 100 = 0 then false
    else if y mod 4 = 0 then true
    else false

fun reasonable_date (date:fecha) =
    let
        val y = #anio date
        val m = #mes date
        val d = #dia date
    in
        y >= 1 andalso
        m >= 1 andalso m <= 12 andalso
        d >= 1 andalso d <= daysInMonth m andalso
        (m <> 2 orelse d <= 29 orelse (d = 29 andalso isBiciesto y))
    end


val fecha1:fecha = {anio=2000,dia=13,mes=10};
val fecha2:fecha = {anio=2004,dia=27,mes=12}; 
val fecha3:fecha = {anio=2003,dia=27,mes=4};
val fecha4:fecha = {anio=2005,dia=15,mes=10};
val fecha5:fecha = {anio=2000,dia=29,mes=13};
val fechas = [fecha1, fecha2, fecha3];

