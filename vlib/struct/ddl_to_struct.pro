pro DDL_to_struct, dataset, VARIANT=variant

;create IDL function code which defines record structure for DATASET.
;check for DDL file (describing DATASET) and invoke DDL_STRUCT function.
;Frank Varosi Sep.1990

;check if IDL structure definition code exists?

	fst = findfile( dataset + "_STR*.PRO" )
	fst = fst(0)

	if strlen( fst ) GT 0 then begin
		message,"struct. def. code "+fst+" already exists",/CON,/INF
		return
	   endif

;check if DDL (Data Def. Lang.) file exists?

	fddl = findfile( dataset + ".DDL" )
	fddl = fddl(0)

	if strlen( fddl ) LE 0 then begin

		openw,Lunw,"DDL.com",/get_Lun
		printf,Lunw,"$ DMU"
		printf,Lunw,"List/ful/out=" + dataset + ".DDL  " + dataset
		printf,Lunw,"exit"
		printf,Lunw,"$ dir/siz/dat  " + dataset + "*"
		close,Lunw

		message,"creating DDL file for: " + dataset,/CON,/INF
		spawn,"@DDL"
		spawn,"delete DDL.com;",/NOWAIT
	   endif

;read DDL file and write IDL structure definition code:

	status = ddl_struct( DDL = dataset ,/ab,/tr, VAR=variant )
return
end
