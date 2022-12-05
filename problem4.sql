select 
    fio as "ФИО",
	cast(b.dt as time) as "Запуск", 
	cast(DATEDIFF(minute,cast('8:00' as time), cast(b.dt as time)) as float)/60 as "Опоздание число",
	CONVERT(varchar(12), DATEADD(minute, DATEDIFF(minute,Cast('8:00' as time), cast(b.dt as time)), 0), 108) as "Опоздание время"    
from 
	b
WHERE 
	(b.code  = 1 and 
	 DATEDIFF(minute, cast('8:00' as time), cast(b.dt as time) )> 0 )
Order by b.dt ASC;