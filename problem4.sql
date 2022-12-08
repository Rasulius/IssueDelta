declare @a table([fio] varchar(255), [hour] datetime, [info] varchar(max))
insert into @a
([fio], [hour], [info])
values
('Иванов И.А.', '3:00:00', '...'),
('Петров П.С.', '4:00:00', '...'),
('Кочкин З.Э.', '5:00:00', '...');

declare @b table([fio] varchar(255), [dt] datetime, [code] int)
insert into @b
([fio], [dt], [code])
values
('Иванов И.А.', '20201006 5:00:00', 1),
('Иванов И.А.', '20201006 6:00:00', 3),
('Иванов И.А.', '20201006 7:00:00', 3),
('Иванов И.А.', '20201006 8:00:00', 9),
('Иванов И.А.', '20201007 5:20:00', 1),
('Иванов И.А.', '20201007 6:00:00', 3),
('Иванов И.А.', '20201007 7:00:00', 3),
('Иванов И.А.', '20201007 8:00:00', 9),
('Иванов И.А.', '20201008 6:10:00', 1),
('Иванов И.А.', '20201008 6:00:00', 3),
('Иванов И.А.', '20201008 7:00:00', 3),
('Иванов И.А.', '20201008 8:00:00', 9),
('Иванов И.А.', '20201008 8:10:00', 1),
('Иванов И.А.', '20201009 4:50:00', 1),
('Иванов И.А.', '20201009 6:00:00', 3),
('Иванов И.А.', '20201009 7:00:00', 3),
('Иванов И.А.', '20201009 8:00:00', 9),
('Иванов И.А.', '20201009 22:15:00', 1),
('Петров П.С.', '20201005 7:00:00', 1),
('Петров П.С.', '20201005 9:00:00', 2),
('Петров П.С.', '20201005 12:00:00', 3),
('Петров П.С.', '20201005 13:00:00', 5),
('Кочкин З.Э.', '20201004 4:00:00', 1),
('Кочкин З.Э.', '20201004 6:00:00', 6),
('Кочкин З.Э.', '20201004 7:00:00', 4),
('Кочкин З.Э.', '20201004 10:00:00', 7);

SELECT 
	b1.[fio] as "ФИО",
	DATEADD(hh, DATEPART(HOUR, a1.[hour]), [dt]) as "Запуск",
    	Convert(VARCHAR(50), cast(DATEDIFF(minute, DATEADD(hh, DATEPART(HOUR, '8:00'),   CAST( CAST( [dt] AS Date )as datetime)),  DATEADD(hh, DATEPART(HOUR, a1.[hour]), [dt])) as float)/60)   + 'ч' as "Опоздание число",
	CONVERT(varchar(12),  DATEADD(minute, DATEDIFF(minute, DATEADD(hh, DATEPART(HOUR, '8:00'),   CAST( CAST( [dt] AS Date )as datetime)), DATEADD(hh, DATEPART(HOUR, a1.[hour]), [dt])), 0), 108) as "Опоздание время" 
FROM @a a1
RIGHT JOIN @b b1 ON (a1.fio = b1.fio)
WHERE (b1.[code]  = 1 and 
	DATEDIFF(minute,  
			DATEADD(hh, DATEPART(HOUR, '8:00'),   
			CAST( CAST( [dt] AS Date )as datetime)) , 
			DATEADD(hh, DATEPART(HOUR, a1.[hour]), [dt]))> 0 ) 
 
