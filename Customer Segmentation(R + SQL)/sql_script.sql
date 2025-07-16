WITH  cleaned_data as (
	Select 
	o.customerid,
	o.invoiceno,
	o.stockcode,
	o.description,
	o.quantity, 
	o.invoicedate,
	o.unitprice,
	o.country
	from 
	online_retail o 
	Where 
	TRIM(o.customerid) <> ''
  AND o.customerid IS NOT NULL
  AND o.country IS NOT NULL
  AND o.quantity > 0
  AND o.unitprice > 0)
select  customerid,
		round(sum(quantity * unitprice)) AS total_revenue,
		count(DISTINCT invoiceno) AS total_orders,
		sum(quantity) AS total_items,
		MIN(invoicedate) AS first_order_date,
    	MAX(invoicedate) AS last_order_date,
		round(sum(quantity * unitprice) * 1.0/count(DISTINCT invoiceno),3) as avg_order_value,
		round(sum(quantity * unitprice) * 1.0/sum(quantity),3) as avg_item_price,
		country
	from cleaned_data
	group by customerid, country
	order by total_revenue  desc;
