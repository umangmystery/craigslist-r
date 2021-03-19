# craigslist-rhttps://drive.google.com/file/d/1tGx79T6GW3HJtWEVx_0Sk-A11vkDZH9M/view?usp=sharing



Data Processing Done - 
Removed unwanted columns
Removed rows with wrong mentioned regions - 9 columns (number values)
Removed rows with wrong mentioned price - 37 columns (text values)
Removed rows with wrong mentioned years - 1 column (text value)



id : Unique Id of the car
url : Listing Url of the car
region : Craigslist Region
region_url : Region Url
price : Price of the car
year : Year of Sale
manufacturer : Manufacturer of the car
model : Model of the car
condition : Condition the car is in
cylinders : Type of cylinder the car contains
fuel : Fuel Type of the car
odometer : No. of Miles the car traveled
title_status : Status of the vehicle
transmission : Transmission Type
vin : Vehicle Identification Number
drive : Drive Type
size : Size of the car
type : Type of the car
paint_color : Color of the car
image_url : Url of the image of the car
description : Description mentioned by the owner
county : County it belongs to
state : State where it is listed
lat : Latitude of Listing
long : Longitude of Listing
Post Date




Column : ID
To check whether there is any duplicate ID present or not, if yes we can drop the columns of the duplicate ones. But fortunately, there were no Duplicate IDs present in the data.

Column : Price
As we check the price column, we can find 0 values and extreme values as min and max, obviously these values are outliers. Now According to many data available on google, we can say that the price of a used car can be max $150000 depending on cars like Ferarri, Lamborghini, but i would still keep it till $200000 as Max. And lowest being $100. Rest of the amounted rows will be dropped.
Price : Description

Column : Manufacturer
This column contains of different Manufacturers of the Car Brands, All the Values in this column has a not-null value with unique values of 43 count. Hence will be keeping all of them.

Column : Fuel
Talking about fuel, there are mainly 5 fuel types, excluding null values. We remove the rows of null fuel types.

Column : Transmission
This section contains 3 unique values, ‘automatic’, ‘manual’ and ‘other’. Also has null values which is dropped.

Column : State
There are around 51 unique state available, with no null values, hence we will be keeping them all.

Column : Year
Now, year is a very crucial column which might have some good quality of data. We need to be extra careful before dropping any column. There are around 112 unique years available, including 0 and null. The count of 0 and null values are less, hence can be safely removed.

Column : Odometer
This is a numerical value which states, the running distance of the car, with null values, we can remove them, or replace them with 0. Choice is yours.

Column : Drive, Type, Paint and Condition
These are the last 4 categorical columns, we can remove the null values, and keep the rest.


Apart from these columns rest of the columns will be dropped, you can choose to keep them. I choose these as it seems to be the most valued ones needed for prediction.

