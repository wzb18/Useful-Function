--- check data(city, month) in database
select sp.city, sp.date, m.city_name
from (select city_code,city_name from ss_grid_wgs84 group by city_code,city_name) m 
inner join (select city,date from stay_poi group by city,date) sp
on m.city_code = sp.city
group by sp.city, sp.date, m.city_name;
