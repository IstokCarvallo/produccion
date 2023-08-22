$PBExportHeader$w_consulta_sistemaestimacion.srw
forward
global type w_consulta_sistemaestimacion from w_mant_tabla
end type
type dw_2 from datawindow within w_consulta_sistemaestimacion
end type
type dw_variedad from uo_dw within w_consulta_sistemaestimacion
end type
type dw_detalle from uo_dw within w_consulta_sistemaestimacion
end type
end forward

global type w_consulta_sistemaestimacion from w_mant_tabla
integer width = 4928
integer height = 1888
string title = "Consulta Sistema Estimaciones"
boolean hscrollbar = true
integer unitspercolumn = 1
event ue_asignacion ( )
event ue_asignacion_enca ( )
dw_2 dw_2
dw_variedad dw_variedad
dw_detalle dw_detalle
end type
global w_consulta_sistemaestimacion w_consulta_sistemaestimacion

type variables
//

uo_productores 			iuo_productor
uo_agronomo_productor	iuo_agroespeprod
uo_especie 					iuo_especie
uo_variedades				iuo_variedad
str_busqueda 				istr_anfin

DataWindowChild	idwc_Productor, idwc_Variedad, idwc_agronomo, idwc_Especie, idwc_Packing

DataStore ids_1, ids_2

Integer ii_filadc = 0


end variables

forward prototypes
public function boolean existecoordinador (integer ai_coordina)
public function long recuperainfoplantas (integer ai_temporada, integer ai_agronomo, integer ai_productor, integer ai_especie, integer ai_variedad, integer ai_zona, integer ai_grupoprod, integer ai_coordina, datetime ad_fecha)
public subroutine informerecepcionlote ()
public subroutine procesoinfoprod ()
public subroutine procesoinfogral ()
public subroutine informeresumen ()
public subroutine basesconec (ref string as_bases[])
public subroutine procesoinfo (boolean ab_variedad)
end prototypes

event ue_asignacion();SetPointer(HourGlass!)

Long		fila, ll_predio
Integer  li_productor, li_especie, li_agronomo, li_variedad
Str_busqueda	lstr_busq

li_productor = dw_1.Object.prod_codigo[il_fila]
li_especie   = dw_1.Object.espe_codigo[il_fila]
li_variedad  = dw_1.Object.vari_codigo[il_fila]
li_agronomo  = dw_1.Object.agro_codigo[il_fila]
IF dw_2.Object.tipo_selecc[1] = 0 THEN
	ll_predio = dw_1.Object.prpr_codigo[il_fila]
END IF

IF gi_tiposel = 11 THEN
	informerecepcionlote()
ELSEIF gi_tiposel = 12 THEN


ELSEIF gi_tiposel = 2 THEN
	
ELSEIF gi_tiposel = 31 THEN
	

ELSEIF gi_tiposel = 32 THEN

	
	istr_info.titulo	= "FICHA CATASTRO"
	
	istr_info.copias	= 1
	
	OpenWithParm(vinf, istr_info)
	
	
	vinf.dw_1.DataObject = "dw_info_prodcuarteles_pronostico"
	
	vinf.dw_1.SetTransObject(sqlca)
	
	fila = vinf.dw_1.Retrieve(li_productor,ll_predio,li_especie,li_agronomo,li_variedad)
	
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF

ELSEIF gi_tiposel = 33 THEN
	
	SetPointer(HourGlass!)
	gstr_us.OpcionActiva	=	'm_consultacatastrosuperficie'

	OpenSheet(w_consulta_catastro_superficie, w_main, 7, Original!)
	
ELSEIF gi_tiposel = 41 THEN

	lstr_Busq.Argum[1]	=	String(dw_1.Object.agro_codigo[il_fila])
	lstr_Busq.Argum[2]	=	String(dw_1.object.prod_codigo[il_fila])
	lstr_Busq.Argum[3]	=	String(dw_1.object.prpr_codigo[il_fila])
	lstr_Busq.Argum[4]	=	String(dw_1.object.espe_codigo[il_fila])
	lstr_Busq.Argum[5]	=	String(dw_1.object.vari_codigo[il_fila])

	gstr_us.OpcionActiva	=	'm_fichapronóstico'
	
	OpenSheetWithParm(w_plan_pronostico_cosecha, lstr_busq, w_main, 7 , Original!)
	

ELSEIF gi_tiposel = 42 THEN
	
	istr_info.titulo	= "FICHA PRONOSTICO"
	istr_info.copias	= 1

   OpenWithParm(vinf,istr_info)

	vinf.dw_1.DataObject = "dw_info_ficha_pronostico_comp"

	vinf.dw_1.SetTransObject(sqlca)

	fila = vinf.dw_1.Retrieve(li_productor,ll_predio,li_especie,li_variedad,gstr_tempo.temporada)
									  
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	ELSE
		vinf.dw_1.Modify("especie.text = '" + dw_1.Object.espe_nombre[il_fila] + "'")
		vinf.dw_1.Modify("variedad.text = '" + dw_1.Object.vari_nombre[il_fila] + "'")
		vinf.dw_1.Modify("productor.text = '" + dw_1.Object.prod_nombre[il_fila] + "'")	
		vinf.dw_1.Modify("predio.text = '" + dw_1.Object.prbr_predio[il_fila] + "'")
		vinf.dw_1.Modify("agronomo.text = '" + dw_1.Object.agro_nombre[il_fila] + "'")
		vinf.dw_1.Modify("packing.text = '" + dw_1.Object.plde_nombre[il_fila] + "'")

		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF

ELSEIF gi_tiposel = 43 THEN	
		
	istr_info.titulo	= "INFORME PRONOSTICO COSECHA"
	istr_info.copias	= 1

   OpenWithParm(vinf,istr_info)

	vinf.dw_1.DataObject = "dw_info_pron_cosecha_zona_final"

	vinf.dw_1.SetTransObject(sqlca)

	fila = vinf.dw_1.Retrieve(li_especie,li_variedad,gstr_tempo.temporada)
									  
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF
	
ELSEIF gi_tiposel = 5 THEN
	
END IF

SetPointer(Arrow!)
end event

event ue_asignacion_enca();SetPointer(HourGlass!)

Long		fila, ll_predio
Integer  li_productor, li_especie, li_agronomo, li_variedad, li_zona, li_grupoprod, &
			li_coordina, li_planta, li_sema1, li_sema2, li_temporada
Datetime ld_fecha

Str_busqueda	lstr_busq

dw_2.Accepttext()

li_productor = dw_2.Object.prod_codigo[1]
li_especie   = dw_2.Object.espe_codigo[1]
li_variedad  = dw_2.Object.vari_codigo[1]
li_agronomo  = dw_2.Object.agro_codigo[1]
	
IF gi_tiposel = 32 THEN
	istr_info.titulo	= "FICHA CATASTRO"
	
	istr_info.copias	= 1
	
	OpenWithParm(vinf, istr_info)
	
	
	vinf.dw_1.DataObject = "dw_info_prodcuarteles_pronostico"
	
	vinf.dw_1.SetTransObject(sqlca)
	
	fila = vinf.dw_1.Retrieve(li_productor,ll_predio,li_especie,li_agronomo,li_variedad)
	
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF

ELSEIF gi_tiposel = 42 THEN
	
	istr_info.titulo	= "FICHA PRONOSTICO"
	istr_info.copias	= 1

   OpenWithParm(vinf,istr_info)

	vinf.dw_1.DataObject = "dw_info_ficha_pronostico_comp"

	vinf.dw_1.SetTransObject(sqlca)

	fila = vinf.dw_1.Retrieve(li_productor,ll_predio,&
   	                       li_especie,li_variedad,&
									  gstr_tempo.temporada)
									  
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		vinf.dw_1.Modify("especie.text = '" + dw_1.Object.espe_nombre[il_fila] + "'")
		vinf.dw_1.Modify("variedad.text = '" + dw_1.Object.vari_nombre[il_fila] + "'")
		vinf.dw_1.Modify("productor.text = '" + dw_1.Object.prod_nombre[il_fila] + "'")	
	
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF
ELSEIF gi_tiposel = 41 THEN

	OpenSheetWithParm(w_plan_pronostico_cosecha, lstr_busq, w_main, 7 , Original!)

ELSEIF gi_tiposel = 43 THEN	
	
	istr_info.titulo	= "INFORME PRONOSTICO COSECHA"
	istr_info.copias	= 1
	
	IF dw_2.Object.todosespe[1] = 1 THEN
		li_especie = 0
	ELSE
		li_especie = dw_2.Object.espe_codigo[1]
	END IF
	
	IF dw_2.Object.todosvari[1] = 1 THEN
		li_variedad = 0
	ELSE
		li_variedad = dw_2.Object.vari_codigo[1]
	END IF
	
   OpenWithParm(vinf,istr_info)

	vinf.dw_1.DataObject = "dw_info_pron_cosecha_zona_final"

	vinf.dw_1.SetTransObject(sqlca)

	fila = vinf.dw_1.Retrieve(li_especie,li_variedad,gstr_tempo.temporada)
									  
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF	
ELSEIF gi_tiposel = 31 THEN
	
ELSEIF gi_tiposel = 33 THEN
	
	SetPointer(HourGlass!)
	gstr_us.OpcionActiva	=	This.ClassName()

	OpenSheet(w_consulta_catastro_superficie, w_main, 7, Original!)

ELSEIF gi_tiposel = 2 THEN
	procesoinfogral()

ELSEIF gi_tiposel = 5 THEN
	
	IF dw_2.Object.todosagro[1] = 1 THEN
		li_agronomo = -1
	ELSE
		li_agronomo = dw_2.Object.agro_codigo[1]
	END IF 
	
	IF dw_2.Object.todosespe[1] = 1 THEN
		li_especie = -1
	ELSE
		li_especie = dw_2.Object.espe_codigo[1]
	END IF
	
	IF dw_2.Object.todosvari[1] = 1 THEN
		li_variedad = -1
	ELSE
		li_variedad = dw_2.Object.vari_codigo[1]
	END IF
	
	IF dw_2.Object.todosprod[1] = 1 THEN
		li_productor = -1
	ELSE
		li_productor = dw_2.Object.prod_codigo[1]
	END IF
	
	IF dw_2.Object.todoszona[1] = 1 THEN
		li_zona = -1
	ELSE
		li_zona = dw_2.Object.zona_codigo[1]
	END IF
	
	IF dw_2.Object.grupo_prod[1] = 1 THEN
		li_productor = -1
		li_grupoprod = iuo_Productor.grupoprod
	ELSE
		li_grupoprod = -1
	END IF
	
	IF dw_2.Object.todoscoor[1] = 1 THEN
		li_coordina = -1
	ELSE
		li_coordina = dw_2.Object.agro_coordi[1]
	END IF
	
	IF dw_2.Object.todosplan[1] = 1 THEN
		li_planta = -1
	ELSE
		li_planta = dw_2.Object.plde_codigo[1]
	END IF
	
	li_sema1 = dw_2.Object.semana_ini[1]
	li_sema2 = dw_2.Object.semana_fin[1]
	ld_fecha = dw_2.Object.rece_bfecha[1]
	
	li_temporada = gstr_tempo.Temporada

	istr_info.titulo	= "FLUJO COSECHA"
	
	istr_info.copias	= 1
	
	OpenWithParm(vinf, istr_info)
		
	vinf.dw_1.DataObject = "dw_consulta_programacosechasemana"
	
	vinf.dw_1.SetTransObject(sqlca)

	fila	= vinf.dw_1.Retrieve(li_sema1,li_sema2,li_temporada, li_agronomo, li_productor,-1,li_planta,&
									 li_especie,li_variedad,li_zona, li_grupoprod, li_coordina)
	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("DataWindow.Header.Height=250")
		IF dw_2.Object.sele_resume[1] = 1 THEN
			vinf.dw_1.Object.DataWindow.Detail.Height = 0
			vinf.dw_1.Modify("DataWindow.Header.2.Height=0")
		ELSE
			vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
			vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
		END IF
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF	
END IF

SetPointer(Arrow!)
end event

public function boolean existecoordinador (integer ai_coordina);Integer li_cuenta

SELECT	Count(agro_coordi) INTO :li_cuenta
	FROM	dba.productores
	WHERE	agro_coordi = :ai_coordina;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Productores")
	RETURN False
END IF

IF isnull(li_cuenta) OR li_cuenta <= 0 THEN RETURN FALSE

RETURN TRUE
end function

public function long recuperainfoplantas (integer ai_temporada, integer ai_agronomo, integer ai_productor, integer ai_especie, integer ai_variedad, integer ai_zona, integer ai_grupoprod, integer ai_coordina, datetime ad_fecha);String	ls_bases[]
Long		ll_Filas, ll_Fila
Integer	li_base, li_atributovisual

Transaction	ltr_plantas
	
ltr_plantas	=	Create Transaction
	
BasesConec(ls_bases)

li_atributovisual = dw_2.Object.sele_atributo[1]

/*Base Local*/
dw_1.SetTransObject(SQLCA)

ll_Fila	=	dw_1.Retrieve(ai_temporada, ai_agronomo, ai_productor, ai_especie, &
								 ai_variedad, ai_zona, ai_grupoprod, ai_coordina, ad_fecha, &
								 li_atributovisual, SQLCA.ServerName, SQLCA.DataBase)

ll_Filas = ll_Filas + ll_Fila

FOR li_base = 1 TO UpperBound(ls_bases)
	ltr_plantas.ServerName	=	ProfileString(gstr_apl.ini, ls_bases[li_base], "servername", "")
	ltr_plantas.DataBase		=	ProFileString(gstr_apl.ini, ls_bases[li_base], "database", "")
	ltr_plantas.Dbms			=	SQLCA.Dbms
	ltr_plantas.Autocommit	=	True
	ltr_plantas.logid			=	gstr_apl.usuarioremoto
	ltr_plantas.logpass			=	gstr_apl.passremoto
		
	CONNECT Using ltr_plantas ; 
	
	IF ltr_plantas.SQLCode <> 0 THEN
		IF ltr_plantas.SQLDBCode <> 0 THEN
			MessageBox("Atención","No se pudo conectar a la Base " + ls_bases[li_base] + &
							". Informe no contendra información de esta base.")
		END IF
	ELSE
			
		dw_1.SetTransObject(ltr_plantas)
			
		ll_Fila	=	dw_1.Retrieve(ai_temporada, ai_agronomo, ai_productor, ai_especie, &
		                         ai_variedad, ai_zona, ai_grupoprod, ai_coordina, ad_fecha, &
										 li_atributovisual, ltr_plantas.ServerName, ltr_plantas.DataBase)
			
		IF ll_Fila = -1 THEN
			MessageBox( "Atención", "No es Posible Rescatar Información desde " + &
			"Base de datos : " + ls_bases[li_base] + ".")
		ELSE
			ll_Filas = ll_Filas + ll_Fila
		END IF

	END IF
		
	DISCONNECT Using ltr_plantas ; 
NEXT			

dw_1.GroupCalc()

Destroy ltr_plantas

RETURN ll_Filas
end function

public subroutine informerecepcionlote ();Long fila
Datetime ld_fechaini, ld_fechafin
Integer li_agro, li_productor

Transaction	ltr_plantas

ltr_plantas	=	Create Transaction

ltr_plantas.ServerName	=	dw_1.Object.server_name[il_fila]
ltr_plantas.DataBase		=	dw_1.Object.data_base[il_fila]
ltr_plantas.Dbms			=	SQLCA.Dbms
ltr_plantas.Autocommit	=	True
ltr_plantas.logid			=	gstr_apl.usuarioremoto
ltr_plantas.logpass		=	gstr_apl.passremoto

CONNECT Using ltr_plantas ; 
	
IF ltr_plantas.SQLCode <> 0 THEN
	IF ltr_plantas.SQLDBCode <> 0 THEN
		MessageBox("Atención","No se pudo conectar a la Base " + ltr_plantas.DataBase + &
						". Informe no contendra información de esta base.")
	END IF
ELSE

	ld_fechafin = Datetime(Date(String(Today(),'dd/mm/yyyy')),Time('23:59'))
	ld_fechaini = dw_2.Object.rece_bfecha[1]
	
	istr_info.titulo	= "ANALISIS CONTROL DE CALIDAD - LOTES"
	istr_info.copias	= 1
	
	OpenWithParm(vinf,istr_info)
	
	vinf.dw_1.DataObject = "dw_info_lotesrecepcion"
	
	vinf.dw_1.SetTransObject(ltr_plantas)
	
	IF dw_2.Object.sele_atributo[1] = 1 THEN
		li_productor = -1
	ELSE
		li_productor = dw_1.Object.prod_codigo[il_fila]
	END IF
	
	fila = vinf.dw_1.Retrieve(dw_1.Object.agro_codigo[il_fila],dw_1.Object.plde_codigo[il_fila],&
									  dw_1.Object.espe_codigo[il_fila],dw_1.Object.vari_codigo[il_fila],&
									  li_productor,ld_fechaini, ld_fechafin,dw_1.Object.prbr_codpre[il_fila] )
	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
							StopSign!, Ok!)
	ELSE
			
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
		
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF
END IF

DISCONNECT Using ltr_plantas ; 
	
Destroy ltr_plantas 	
	
end subroutine

public subroutine procesoinfoprod ();Integer	li_Planta, li_Especie, li_Zona, li_GrupoProd, &
			li_Variedad, li_Coordina,li_Fila, li_Productor, li_agronomo, &
			li_base=0
Datetime ldt_Fechadesde, ldt_FechaHasta
String	ls_fecha, ls_bases[]

DataWindowChild	ldwc_proceso

SetPointer(HourGlass!)

dw_detalle.DataObject = 'dw_info_resumen_proceso_productor'

ids_1.DataObject = dw_detalle.DataObject

ids_1.SetTransObject(SQLCA)
ids_1.Reset()

dw_detalle.SetTransObject(SQLCA)
dw_detalle.Reset()

dw_2.accepttext()

li_Planta = -1

IF dw_2.Object.todosagro[1] = 1 THEN
	li_agronomo = -1
ELSE
	li_agronomo = dw_2.Object.agro_codigo[1]
END IF 

IF dw_2.Object.todosespe[1] = 1 THEN
	li_especie = -1
ELSE
	li_especie = dw_2.Object.espe_codigo[1]
END IF

IF dw_2.Object.todosvari[1] = 1 THEN
	li_variedad = -1
ELSE
	li_variedad = dw_2.Object.vari_codigo[1]
END IF

IF dw_2.Object.todosprod[1] = 1 THEN
	li_productor = -1
ELSE
	li_productor = dw_2.Object.prod_codigo[1]
END IF

IF dw_2.Object.todoszona[1] = 1 THEN
	li_zona = -1
ELSE
	li_zona = dw_2.Object.zona_codigo[1]
END IF

IF dw_2.Object.grupo_prod[1] = 1 THEN
	li_productor = -1
	li_grupoprod = iuo_Productor.grupoprod
ELSE
	li_grupoprod = -1
END IF

IF dw_2.Object.todoscoor[1] = 1 THEN
	li_coordina = -1
ELSE
	li_coordina = dw_2.Object.agro_coordi[1]
END IF

ldt_fechadesde = Datetime(gstr_tempo.fechainicio)
ldt_fechahasta = Datetime(Date(String(Today(),'dd/mm/yyyy')),Time('23:59'))
						 
dw_detalle.Retrieve(li_planta, li_Especie,li_Variedad, li_Productor, li_agronomo, &
						  li_zona, li_GrupoProd, li_Coordina, ldt_fechadesde, ldt_fechahasta, &
						  -1, dw_2.Object.sele_atributo[1])

Transaction	ltr_plantas
	
ltr_plantas	=	Create Transaction
	
BasesConec(ls_bases)
	
FOR li_base = 1 TO UpperBound(ls_bases)
	ltr_plantas.ServerName	=	ProfileString(gstr_apl.ini, ls_bases[li_base], "servername", "")
	ltr_plantas.DataBase		=	ProFileString(gstr_apl.ini, ls_bases[li_base], "database", "")
	ltr_plantas.Dbms			=	SQLCA.Dbms
	ltr_plantas.Autocommit	=	True
	ltr_plantas.logid			=	gstr_apl.usuarioremoto
	ltr_plantas.logpass		=	gstr_apl.passremoto
		
	CONNECT Using ltr_plantas ; 
	
	IF ltr_plantas.SQLCode <> 0 THEN
		IF ltr_plantas.SQLDBCode <> 0 THEN
			MessageBox("Atención","No se pudo conectar a la Base " + ls_bases[li_base] + &
							". Informe no contendra información de esta base.")
		END IF
	ELSE
		
		ids_1.SetTransObject(ltr_plantas)
		
		li_fila	=	ids_1.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
        							      li_zona, li_GrupoProd, li_Coordina,ldt_fechadesde, ldt_fechahasta, &
											-1, dw_2.Object.sele_atributo[1])
			
		IF li_Fila = -1 THEN
				MessageBox( "Atención", "No es Posible Rescatar Información desde " + &
					"Base de datos : " + ls_bases[li_base] + ".")
		END IF

			
		ids_1.GroupCalc()				  

			
		Procesoinfo(FALSE)
			
	END IF
		
	DISCONNECT Using ltr_plantas ; 
NEXT			
	
Destroy ltr_plantas 	
	
dw_detalle.GroupCalc()			
	
li_fila =	dw_detalle.RowCount()
	
IF li_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF li_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
		
	dw_detalle.RowsCopy (1, dw_detalle.RowCount(), primary!, dw_1, 1, primary! )
		
	dw_1.Sort()
	dw_1.GroupCalc()
		
END IF

dw_detalle.SetTransObject(SQLCA)

SetPointer(Arrow!)
end subroutine

public subroutine procesoinfogral ();Integer	li_Planta, li_Especie, li_Zona, li_GrupoProd, &
			li_Variedad, li_Coordina,li_Fila, li_Productor, li_agronomo, &
			li_base=0
Datetime ldt_Fechadesde, ldt_FechaHasta
String	ls_fecha, ls_bases[]

DataWindowChild	ldwc_proceso, ldwc_variedad

SetPointer(HourGlass!)

dw_detalle.DataObject = 'dw_info_resumen_proceso_productor'

ids_1.DataObject = dw_detalle.DataObject

ids_1.SetTransObject(SQLCA)
ids_1.Reset()

ids_2.DataObject = dw_variedad.DataObject

ids_2.SetTransObject(SQLCA)
ids_2.Reset()

dw_detalle.SetTransObject(SQLCA)
dw_detalle.Reset()

dw_variedad.SetTransObject(SQLCA)
dw_variedad.Reset()

istr_info.titulo	= 'RESUMEN DE PROCESOS POR PRODUCTOR'

dw_1.accepttext()

li_Planta = -1

IF dw_2.Object.todosagro[1] = 1 THEN
	li_agronomo = -1
ELSE
	li_agronomo = dw_2.Object.agro_codigo[1]
END IF 

IF dw_2.Object.todosespe[1] = 1 THEN
	li_especie = -1
ELSE
	li_especie = dw_2.Object.espe_codigo[1]
END IF

IF dw_2.Object.todosvari[1] = 1 THEN
	li_variedad = -1
ELSE
	li_variedad = dw_2.Object.vari_codigo[1]
END IF

IF dw_2.Object.todosprod[1] = 1 THEN
	li_productor = -1
ELSE
	li_productor = dw_2.Object.prod_codigo[1]
END IF

IF dw_2.Object.todoszona[1] = 1 THEN
	li_zona = -1
ELSE
	li_zona = dw_2.Object.zona_codigo[1]
END IF

IF dw_2.Object.grupo_prod[1] = 1 THEN
	li_productor = -1
	li_grupoprod = iuo_Productor.grupoprod
ELSE
	li_grupoprod = -1
END IF

IF dw_2.Object.todoscoor[1] = 1 THEN
	li_coordina = -1
ELSE
	li_coordina = dw_2.Object.agro_coordi[1]
END IF

ldt_fechadesde = Datetime(gstr_tempo.fechainicio)
ldt_fechahasta = Datetime(Date(String(Today(),'dd/mm/yyyy')),Time('23:59'))

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject =	"dw_info_resumen_proc_prod_consol"

vinf.dw_1.SetTransObject(SQLCA)
	
vinf.dw_1.Retrieve(-7, -7, -7, -7, li_agronomo,-7, &
                   -7, -7, ldt_fechadesde, ldt_fechahasta,-7, 0)
										 
vinf.dw_1.GetChild("dw_resumen",ldwc_proceso)
vinf.dw_1.GetChild("dw_detalle",ldwc_variedad)
	
GarbageCollect()
	
	dw_detalle.Retrieve(li_planta, li_Especie,li_Variedad, li_Productor, li_agronomo, &
							  li_zona, li_GrupoProd, li_Coordina, ldt_fechadesde, ldt_fechahasta, &
							  -1, dw_2.Object.sele_atributo[1])

	dw_variedad.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
							   li_zona, li_GrupoProd, li_Coordina, ldt_fechadesde, ldt_fechahasta, &
								-1)

	Transaction	ltr_plantas
	
	ltr_plantas	=	Create Transaction
	
	BasesConec(ls_bases)
	
	FOR li_base = 1 TO UpperBound(ls_bases)
		ltr_plantas.ServerName	=	ProfileString(gstr_apl.ini, ls_bases[li_base], "servername", "")
		ltr_plantas.DataBase		=	ProFileString(gstr_apl.ini, ls_bases[li_base], "database", "")
		ltr_plantas.Dbms			=	SQLCA.Dbms
		ltr_plantas.Autocommit	=	True
		ltr_plantas.logid			=	gstr_apl.usuarioremoto
		ltr_plantas.logpass		=	gstr_apl.passremoto
		
		CONNECT Using ltr_plantas ; 
	
		IF ltr_plantas.SQLCode <> 0 THEN
			IF ltr_plantas.SQLDBCode <> 0 THEN
				MessageBox("Atención","No se pudo conectar a la Base " + ls_bases[li_base] + &
								". Informe no contendra información de esta base.")
			END IF
		ELSE
			
			ids_1.SetTransObject(ltr_plantas)
			
			li_fila	=	ids_1.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
        							         li_zona, li_GrupoProd, li_Coordina,ldt_fechadesde, ldt_fechahasta, &
												-1, dw_2.Object.sele_atributo[1])
			
			IF li_Fila = -1 THEN
					MessageBox( "Atención", "No es Posible Rescatar Información desde " + &
						"Base de datos : " + ls_bases[li_base] + ".")
			END IF

			ids_2.SetTransObject(ltr_plantas)
			
			li_fila	=	li_fila + ids_2.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
							                         li_zona, li_GrupoProd, li_Coordina,ldt_fechadesde, ldt_fechahasta, &
															 -1)

			IF li_Fila = -1 THEN
					MessageBox( "Atención", "No es Posible Rescatar Información desde " + &
						"Base de datos : " + ls_bases[li_base] + ".")
			END IF
			
			ids_1.GroupCalc()				  
			ids_2.GroupCalc()
			
			Procesoinfo(TRUE)
			
		END IF
		
		DISCONNECT Using ltr_plantas ; 
	NEXT			
	
	Destroy ltr_plantas 	
	
	dw_detalle.GroupCalc()			
	dw_variedad.GroupCalc()
	
	li_fila =	dw_detalle.RowCount()
	


IF li_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF li_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa 
	vinf.dw_1.Object.dw_detalle.Object.referencia.text		= gstr_apl.referencia
	vinf.dw_1.Object.dw_resumen.Object.nom_empresa.text	= gstr_apl.nom_empresa 
	vinf.dw_1.Object.dw_resumen.Object.referencia.text		= gstr_apl.referencia
		
	dw_variedad.RowsCopy (1, dw_variedad.RowCount(), primary!, ldwc_variedad, 1, primary! )
	dw_detalle.RowsCopy (1, dw_detalle.RowCount(), primary!, ldwc_proceso, 1, primary! )
		
	ldwc_variedad.Sort()
	ldwc_proceso.Sort()
	ldwc_variedad.GroupCalc()
	ldwc_proceso.GroupCalc()
		
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

dw_detalle.SetTransObject(SQLCA)

SetPointer(Arrow!)
end subroutine

public subroutine informeresumen ();Integer	li_Planta, li_Especie, li_Zona, li_GrupoProd, &
			li_Variedad, li_Coordina,li_Fila, li_Productor, li_agronomo, &
			li_base=0, li_atributo
Datetime ldt_Fechadesde, ldt_FechaHasta
String	ls_fecha, ls_bases[]
Long		ll_predio

DataWindowChild	ldwc_proceso, ldwc_variedad

SetPointer(HourGlass!)

dw_detalle.DataObject = 'dw_info_resumen_proceso_productor'

ids_1.DataObject = dw_detalle.DataObject

ids_1.SetTransObject(SQLCA)
ids_1.Reset()

ids_2.DataObject = dw_variedad.DataObject

ids_2.SetTransObject(SQLCA)
ids_2.Reset()

dw_detalle.SetTransObject(SQLCA)
dw_detalle.Reset()

dw_variedad.SetTransObject(SQLCA)
dw_variedad.Reset()

istr_info.titulo	= 'RESUMEN DE PROCESOS POR PRODUCTOR'

dw_1.accepttext()

li_Planta = dw_1.Object.plde_codigo[il_fila]

li_Especie = dw_1.Object.espe_codigo[il_fila]

li_Variedad = dw_1.Object.vari_codigo[il_fila]

li_agronomo = dw_1.Object.agro_codigo[il_fila]

ll_predio	= dw_1.Object.prbr_codpre[il_fila]

IF dw_2.Object.todoszona[1] = 1 THEN
	li_zona = -1
ELSE
	li_zona = dw_2.Object.zona_codigo[1]
END IF

IF dw_2.Object.sele_atributo[1] = 1 THEN
	li_Productor = -1
	li_grupoprod = dw_1.Object.prod_codigo[il_fila]
ELSE
	li_Productor = dw_1.Object.prod_codigo[il_fila]
	li_grupoprod = -1
END IF	

IF dw_2.Object.todoscoor[1] = 1 THEN
	li_coordina = -1
ELSE
	li_coordina = dw_2.Object.agro_coordi[1]
END IF

li_atributo = dw_2.Object.sele_atributo[1]
//Paramtemporada(gstr_paramtempo)

ldt_fechadesde = Datetime(gstr_tempo.fechainicio)
//ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ldt_fechahasta = Datetime(Date(String(Today(),'dd/mm/yyyy')),Time('23:59'))

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject =	"dw_info_resumen_proc_prod_consol"

vinf.dw_1.SetTransObject(SQLCA)
	
vinf.dw_1.Retrieve(-7, -7, -7, -7, li_agronomo,-7, &
                   -7, -7, ldt_fechadesde, ldt_fechahasta,-7,-7)
										 
vinf.dw_1.GetChild("dw_resumen",ldwc_proceso)
vinf.dw_1.GetChild("dw_detalle",ldwc_variedad)
	
GarbageCollect()
	
dw_detalle.Retrieve(li_planta, li_Especie,li_Variedad, li_Productor, li_agronomo, &
						  li_zona, li_GrupoProd, li_Coordina, ldt_fechadesde, ldt_fechahasta, &
						  ll_predio, li_atributo)

dw_variedad.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
						   li_zona, li_GrupoProd, li_Coordina, ldt_fechadesde, ldt_fechahasta, &
							ll_predio)

Transaction	ltr_plantas
	
ltr_plantas	=	Create Transaction
	
BasesConec(ls_bases)
	
FOR li_base = 1 TO UpperBound(ls_bases)
		ltr_plantas.ServerName	=	ProfileString(gstr_apl.ini, ls_bases[li_base], "servername", "")
		ltr_plantas.DataBase		=	ProFileString(gstr_apl.ini, ls_bases[li_base], "database", "")
		ltr_plantas.Dbms			=	SQLCA.Dbms
		ltr_plantas.Autocommit	=	True
		ltr_plantas.logid			=	gstr_apl.usuarioremoto
		ltr_plantas.logpass		=	gstr_apl.passremoto
		
		CONNECT Using ltr_plantas ; 
	
		IF ltr_plantas.SQLCode <> 0 THEN
			IF ltr_plantas.SQLDBCode <> 0 THEN
				MessageBox("Atención","No se pudo conectar a la Base " + ls_bases[li_base] + &
								". Informe no contendra información de esta base.")
			END IF
		ELSE
			
			ids_1.SetTransObject(ltr_plantas)
			
			li_fila	=	ids_1.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
        							         li_zona, li_GrupoProd, li_Coordina,ldt_fechadesde, ldt_fechahasta, &
												ll_predio, li_atributo)
			
			IF li_Fila = -1 THEN
					MessageBox( "Atención", "No es Posible Rescatar Información desde " + &
						"Base de datos : " + ls_bases[li_base] + ".")
			END IF

			ids_2.SetTransObject(ltr_plantas)
			
			li_fila	=	li_fila + ids_2.Retrieve(li_planta,li_Especie,li_Variedad, li_Productor, li_agronomo, &
							                         li_zona, li_GrupoProd, li_Coordina,ldt_fechadesde, ldt_fechahasta, &
															 ll_predio)

			IF li_Fila = -1 THEN
					MessageBox( "Atención", "No es Posible Rescatar Información desde " + &
						"Base de datos : " + ls_bases[li_base] + ".")
			END IF
			
			ids_1.GroupCalc()				  
			ids_2.GroupCalc()
			
			Procesoinfo(TRUE)
			
		END IF
		
		DISCONNECT Using ltr_plantas ; 
	NEXT			
	
	Destroy ltr_plantas 	
	
	dw_detalle.GroupCalc()			
	dw_variedad.GroupCalc()
	
	li_fila =	dw_detalle.RowCount()
	


IF li_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF li_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa 
	vinf.dw_1.Object.dw_detalle.Object.referencia.text		= gstr_apl.referencia
	vinf.dw_1.Object.dw_resumen.Object.nom_empresa.text	= gstr_apl.nom_empresa 
	vinf.dw_1.Object.dw_resumen.Object.referencia.text		= gstr_apl.referencia
		
	dw_variedad.RowsCopy (1, dw_variedad.RowCount(), primary!, ldwc_variedad, 1, primary! )
	dw_detalle.RowsCopy (1, dw_detalle.RowCount(), primary!, ldwc_proceso, 1, primary! )
		
	ldwc_variedad.Sort()
	ldwc_proceso.Sort()
	ldwc_variedad.GroupCalc()
	ldwc_proceso.GroupCalc()
		
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

dw_detalle.SetTransObject(SQLCA)

SetPointer(Arrow!)
end subroutine

public subroutine basesconec (ref string as_bases[]);Integer	li_archivo, li_inicio, li_termino, li_Cont, li_base
String	ls_linea, ls_base, ls_basetemp[], ls_Act, ls_temporada, ls_temlinea

li_archivo				=	FileOpen(gstr_apl.Ini)

DO WHILE FileRead(li_archivo,ls_linea) >= 0
	li_inicio	= Pos(ls_linea,"[",1)
	li_termino	= Pos(ls_linea,"]",1)
	
	IF li_inicio > 0 AND li_termino>0 THEN
		ls_base		=	Mid(ls_linea, li_inicio + 1, li_termino - li_inicio - 1)
		li_Cont	++
		ls_basetemp[li_Cont] = Mid(ls_linea, li_inicio + 1, li_termino - li_inicio - 1)
	END IF
LOOP

FileClose(li_archivo)

li_Cont	=	0

ls_Temporada	=	ProfileString(gstr_apl.ini, gs_base, "TemLinea", "")

FOR li_base	=	1 TO UpperBound(ls_basetemp)
	ls_Act =	ProfileString(gstr_apl.ini, ls_basetemp[li_base], "Consolida", "")
	ls_TemLinea	=	ProfileString(gstr_apl.ini, ls_basetemp[li_base], "TemLinea", "")
	
	IF ls_Act = "1" AND ls_Temporada = ls_TemLinea AND ls_basetemp[li_base] <> gs_base THEN
	
		li_Cont ++
		as_bases[li_Cont] = ls_basetemp[li_base]
	END IF
NEXT


RETURN
end subroutine

public subroutine procesoinfo (boolean ab_variedad);Long ll_fila, ll_fila_b, ll_fila_n 
String ls_especi, ls_produc, ls_varied, ls_agrono, ls_predio
decimal {2} ld_valor1, ld_valor2,ld_valor3, ld_valor4

FOR ll_Fila = 1 TO ids_1.RowCount()
	ls_especi = String(ids_1.Object.espe_codigo[ll_fila])
	ls_produc = String(ids_1.Object.prod_codigo[ll_fila])
	ls_varied = String(ids_1.Object.vari_codigo[ll_fila])
	ls_agrono = String(ids_1.Object.agro_codigo[ll_fila])
	ls_predio = String(ids_1.Object.prbr_codpre[ll_fila])
	
   ll_fila_b = 0
	ll_fila_b = dw_detalle.Find("agro_codigo = "  + ls_agrono + " AND " + &
	                            "espe_codigo = "  + ls_especi + " AND " + &
								 		 "prod_codigo = "  + ls_produc + " AND " + &
										 "prbr_codpre = "  + ls_predio + " AND " + & 
								 		 "vari_codigo = "  + ls_varied, 1, dw_detalle.RowCount())
	
	IF isnull(ll_fila_b) THEN ll_fila_b = 0
	IF ll_fila_b = 0 THEN
		ll_fila_N = dw_detalle.InsertRow(0)
		
		dw_detalle.Object.espe_codigo[ll_fila_N] = ids_1.Object.espe_codigo[ll_fila]
		dw_detalle.Object.espe_nombre[ll_fila_N] = ids_1.Object.espe_nombre[ll_fila]
		dw_detalle.Object.prod_codigo[ll_fila_N] = ids_1.Object.prod_codigo[ll_fila]		
		dw_detalle.Object.prod_nombre[ll_fila_N] = ids_1.Object.prod_nombre[ll_fila]
		dw_detalle.Object.vari_codigo[ll_fila_N] = ids_1.Object.vari_codigo[ll_fila]
		dw_detalle.Object.vari_nombre[ll_fila_N] = ids_1.Object.vari_nombre[ll_fila]
		dw_detalle.Object.tota_kging[ll_fila_N]  = ids_1.Object.tota_kging[ll_fila]
		dw_detalle.Object.tota_biing[ll_fila_N]  = ids_1.Object.tota_biing[ll_fila]
		dw_detalle.Object.tota_kgpro[ll_fila_N]  = ids_1.Object.tota_kgpro[ll_fila]
		dw_detalle.Object.tota_bipro[ll_fila_N]  = ids_1.Object.tota_bipro[ll_fila]
		dw_detalle.Object.tota_kgemb[ll_fila_N]  = ids_1.Object.tota_kgemb[ll_fila]
		dw_detalle.Object.tota_cjemb[ll_fila_N]  = ids_1.Object.tota_cjemb[ll_fila]
		dw_detalle.Object.tota_cjstd[ll_fila_N]  = ids_1.Object.tota_cjstd[ll_fila]
		dw_detalle.Object.tota_poemb[ll_fila_N]  = ids_1.Object.tota_poemb[ll_fila]
		dw_detalle.Object.tota_poest[ll_fila_N]  = ids_1.Object.tota_poest[ll_fila]
		dw_detalle.Object.tota_podif[ll_fila_N]  = ids_1.Object.tota_podif[ll_fila]
		dw_detalle.Object.tota_kgcho[ll_fila_N]  = ids_1.Object.tota_kgcho[ll_fila]
		dw_detalle.Object.tota_cjcho[ll_fila_N]  = ids_1.Object.tota_cjcho[ll_fila]
		dw_detalle.Object.tota_pocho[ll_fila_N]  = ids_1.Object.tota_pocho[ll_fila]
		dw_detalle.Object.tota_kgfdn[ll_fila_N]  = ids_1.Object.tota_kgfdn[ll_fila]
		dw_detalle.Object.tota_cjfdn[ll_fila_N]  = ids_1.Object.tota_cjfdn[ll_fila]
		dw_detalle.Object.tota_pofdn[ll_fila_N]  = ids_1.Object.tota_pofdn[ll_fila]
		dw_detalle.Object.tota_kgcom[ll_fila_N]  = ids_1.Object.tota_kgcom[ll_fila]
		dw_detalle.Object.tota_cjcom[ll_fila_N]  = ids_1.Object.tota_cjcom[ll_fila]
		dw_detalle.Object.tota_pocom[ll_fila_N]  = ids_1.Object.tota_pocom[ll_fila]
		dw_detalle.Object.vari_pnestd[ll_fila_N]  = ids_1.Object.vari_pnestd[ll_fila]
		dw_detalle.Object.agro_codigo[ll_fila_N]  = ids_1.Object.agro_codigo[ll_fila]
		dw_detalle.Object.agro_abrevi[ll_fila_N]  = ids_1.Object.agro_abrevi[ll_fila]
		dw_detalle.Object.prbr_codpre[ll_fila_N]  = ids_1.Object.prbr_codpre[ll_fila]
		
	ELSEIF ll_fila_b > 0 THEN
		
		dw_detalle.Object.tota_kging[ll_fila_b]  = dw_detalle.Object.tota_kging[ll_fila_b] + ids_1.Object.tota_kging[ll_fila]
		dw_detalle.Object.tota_biing[ll_fila_b]  = dw_detalle.Object.tota_biing[ll_fila_b] + ids_1.Object.tota_biing[ll_fila]
		// 05-04-2005 L.C.A.  Los promedios son ponderados 
		ld_valor1=(dw_detalle.Object.tota_kgpro[ll_fila_b] * dw_detalle.Object.tota_poest[ll_fila_b] + &
			             ids_1.Object.tota_kgpro[ll_fila]   *      ids_1.Object.tota_poest[ll_fila] ) / &
					 (dw_detalle.Object.tota_kgpro[ll_fila_b] + ids_1.Object.tota_kgpro[ll_fila])
					 
		ld_valor2=(dw_detalle.Object.tota_kgpro[ll_fila_b] * dw_detalle.Object.tota_poemb[ll_fila_b] + &
			             ids_1.Object.tota_kgpro[ll_fila]   *      ids_1.Object.tota_poemb[ll_fila] ) / &
					 (dw_detalle.Object.tota_kgpro[ll_fila_b] + ids_1.Object.tota_kgpro[ll_fila])
		
		dw_detalle.Object.tota_kgpro[ll_fila_b]  = dw_detalle.Object.tota_kgpro[ll_fila_b] + ids_1.Object.tota_kgpro[ll_fila]
		
		dw_detalle.Object.tota_bipro[ll_fila_b]  = dw_detalle.Object.tota_bipro[ll_fila_b] + ids_1.Object.tota_bipro[ll_fila]
		ld_valor3= dw_detalle.Object.tota_kgemb[ll_fila_b] 
		ld_valor4= ids_1.Object.tota_kgemb[ll_fila]
		
		dw_detalle.Object.tota_kgemb[ll_fila_b]  = dw_detalle.Object.tota_kgemb[ll_fila_b] + ids_1.Object.tota_kgemb[ll_fila]
		dw_detalle.Object.tota_cjemb[ll_fila_b]  = dw_detalle.Object.tota_cjemb[ll_fila_b] + ids_1.Object.tota_cjemb[ll_fila]
		dw_detalle.Object.tota_cjstd[ll_fila_b]  = dw_detalle.Object.tota_cjstd[ll_fila_b] + ids_1.Object.tota_cjstd[ll_fila]
	
	//	dw_detalle.Object.tota_poemb[ll_fila_b]  = dw_detalle.Object.tota_poemb[ll_fila_b] + ids_1.Object.tota_poemb[ll_fila]
	//	dw_detalle.Object.tota_poest[ll_fila_b]  = dw_detalle.Object.tota_poest[ll_fila_b] + ids_1.Object.tota_poest[ll_fila]
	//	dw_detalle.Object.tota_podif[ll_fila_b]  = dw_detalle.Object.tota_podif[ll_fila_b] + ids_1.Object.tota_podif[ll_fila]

		dw_detalle.Object.tota_poemb[ll_fila_b]  = ld_valor2
		dw_detalle.Object.tota_poest[ll_fila_b]  = ld_valor1
		dw_detalle.Object.tota_podif[ll_fila_b]  = ld_valor2 - ld_valor1
		
		dw_detalle.Object.tota_kgcho[ll_fila_b]  = dw_detalle.Object.tota_kgcho[ll_fila_b] + ids_1.Object.tota_kgcho[ll_fila]
		dw_detalle.Object.tota_cjcho[ll_fila_b]  = dw_detalle.Object.tota_cjcho[ll_fila_b] + ids_1.Object.tota_cjcho[ll_fila]
		dw_detalle.Object.tota_pocho[ll_fila_b]  = dw_detalle.Object.tota_pocho[ll_fila_b] + ids_1.Object.tota_pocho[ll_fila]
		dw_detalle.Object.tota_kgfdn[ll_fila_b]  = dw_detalle.Object.tota_kgfdn[ll_fila_b] + ids_1.Object.tota_kgfdn[ll_fila]
		dw_detalle.Object.tota_cjfdn[ll_fila_b]  = dw_detalle.Object.tota_cjfdn[ll_fila_b] + ids_1.Object.tota_cjfdn[ll_fila]
		dw_detalle.Object.tota_pofdn[ll_fila_b]  = dw_detalle.Object.tota_pofdn[ll_fila_b] + ids_1.Object.tota_pofdn[ll_fila]
		dw_detalle.Object.tota_kgcom[ll_fila_b]  = dw_detalle.Object.tota_kgcom[ll_fila_b] + ids_1.Object.tota_kgcom[ll_fila]
		dw_detalle.Object.tota_cjcom[ll_fila_b]  = dw_detalle.Object.tota_cjcom[ll_fila_b] + ids_1.Object.tota_cjcom[ll_fila]
		dw_detalle.Object.tota_pocom[ll_fila_b]  = dw_detalle.Object.tota_pocom[ll_fila_b] + ids_1.Object.tota_pocom[ll_fila]
		
	END IF
NEXT

IF ab_variedad THEN
	FOR ll_Fila = 1 TO ids_2.RowCount()
		ls_especi = String(ids_2.Object.espe_codigo[ll_fila])
		ls_varied = String(ids_2.Object.vari_codigo[ll_fila])
		ls_agrono = String(ids_2.Object.agro_codigo[ll_fila])
		
		ll_fila_b = 0
		ll_fila_b = dw_variedad.Find("espe_codigo = "  + ls_especi + " AND " + &
		                             "agro_codigo = "  + ls_agrono + " AND " + &  
											  "vari_codigo = "  + ls_varied, 1, dw_variedad.RowCount())
		
		IF isnull(ll_fila_b) THEN ll_fila_b = 0
		IF ll_fila_b = 0 THEN
			ll_fila_N = dw_variedad.InsertRow(0)
			
			dw_variedad.Object.espe_codigo[ll_fila_N] = ids_2.Object.espe_codigo[ll_fila]
			dw_variedad.Object.espe_nombre[ll_fila_N] = ids_2.Object.espe_nombre[ll_fila]
			dw_variedad.Object.vari_codigo[ll_fila_N] = ids_2.Object.vari_codigo[ll_fila]
			dw_variedad.Object.vari_nombre[ll_fila_N] = ids_2.Object.vari_nombre[ll_fila]
			dw_variedad.Object.tota_kging[ll_fila_N]  = ids_2.Object.tota_kging[ll_fila]
			dw_variedad.Object.tota_biing[ll_fila_N]  = ids_2.Object.tota_biing[ll_fila]
			dw_variedad.Object.tota_kgpro[ll_fila_N]  = ids_2.Object.tota_kgpro[ll_fila]
			dw_variedad.Object.tota_bipro[ll_fila_N]  = ids_2.Object.tota_bipro[ll_fila]
			dw_variedad.Object.tota_kgemb[ll_fila_N]  = ids_2.Object.tota_kgemb[ll_fila]
			dw_variedad.Object.tota_cjemb[ll_fila_N]  = ids_2.Object.tota_cjemb[ll_fila]
			dw_variedad.Object.tota_cjstd[ll_fila_N]  = ids_2.Object.tota_cjstd[ll_fila]
			dw_variedad.Object.tota_poemb[ll_fila_N]  = ids_2.Object.tota_poemb[ll_fila]
			dw_variedad.Object.tota_poest[ll_fila_N]  = ids_2.Object.tota_poest[ll_fila]
			dw_variedad.Object.tota_podif[ll_fila_N]  = ids_2.Object.tota_podif[ll_fila]
			dw_variedad.Object.tota_kgcho[ll_fila_N]  = ids_2.Object.tota_kgcho[ll_fila]
			dw_variedad.Object.tota_cjcho[ll_fila_N]  = ids_2.Object.tota_cjcho[ll_fila]
			dw_variedad.Object.tota_pocho[ll_fila_N]  = ids_2.Object.tota_pocho[ll_fila]
			dw_variedad.Object.tota_kgfdn[ll_fila_N]  = ids_2.Object.tota_kgfdn[ll_fila]
			dw_variedad.Object.tota_cjfdn[ll_fila_N]  = ids_2.Object.tota_cjfdn[ll_fila]
			dw_variedad.Object.tota_pofdn[ll_fila_N]  = ids_2.Object.tota_pofdn[ll_fila]
			dw_variedad.Object.tota_kgcom[ll_fila_N]  = ids_2.Object.tota_kgcom[ll_fila]
			dw_variedad.Object.tota_cjcom[ll_fila_N]  = ids_2.Object.tota_cjcom[ll_fila]
			dw_variedad.Object.tota_pocom[ll_fila_N]  = ids_2.Object.tota_pocom[ll_fila]
			dw_variedad.Object.vari_pnestd[ll_fila_N] = ids_2.Object.vari_pnestd[ll_fila]
			dw_variedad.Object.agro_codigo[ll_fila_N] = ids_2.Object.agro_codigo[ll_fila]
			dw_variedad.Object.agro_abrevi[ll_fila_N] = ids_2.Object.agro_abrevi[ll_fila]
			
		ELSEIF ll_fila_b > 0 THEN 	
			
			dw_variedad.Object.tota_kging[ll_fila_b]  = dw_variedad.Object.tota_kging[ll_fila_b] + ids_2.Object.tota_kging[ll_fila]
			dw_variedad.Object.tota_biing[ll_fila_b]  = dw_variedad.Object.tota_biing[ll_fila_b] + ids_2.Object.tota_biing[ll_fila]
			
			ld_valor1=(dw_variedad.Object.tota_kgpro[ll_fila_b] * dw_variedad.Object.tota_poest[ll_fila_b] + &
			             ids_2.Object.tota_kgpro[ll_fila]   *      ids_2.Object.tota_poest[ll_fila] ) / &
					 (dw_variedad.Object.tota_kgpro[ll_fila_b] + ids_2.Object.tota_kgpro[ll_fila])
					 
		   ld_valor2=(dw_variedad.Object.tota_kgpro[ll_fila_b] * dw_variedad.Object.tota_poemb[ll_fila_b] + &
			             ids_2.Object.tota_kgpro[ll_fila]   *      ids_2.Object.tota_poemb[ll_fila] ) / &
					 (dw_variedad.Object.tota_kgpro[ll_fila_b] + ids_2.Object.tota_kgpro[ll_fila])

			
			dw_variedad.Object.tota_kgpro[ll_fila_b]  = dw_variedad.Object.tota_kgpro[ll_fila_b] + ids_2.Object.tota_kgpro[ll_fila]
			dw_variedad.Object.tota_bipro[ll_fila_b]  = dw_variedad.Object.tota_bipro[ll_fila_b] + ids_2.Object.tota_bipro[ll_fila]
			dw_variedad.Object.tota_kgemb[ll_fila_b]  = dw_variedad.Object.tota_kgemb[ll_fila_b] + ids_2.Object.tota_kgemb[ll_fila]
			dw_variedad.Object.tota_cjemb[ll_fila_b]  = dw_variedad.Object.tota_cjemb[ll_fila_b] + ids_2.Object.tota_cjemb[ll_fila]
			dw_variedad.Object.tota_cjstd[ll_fila_b]  = dw_variedad.Object.tota_cjstd[ll_fila_b] + ids_2.Object.tota_cjstd[ll_fila]

//			dw_variedad.Object.tota_poemb[ll_fila_b]  = dw_variedad.Object.tota_poemb[ll_fila_b] + ids_2.Object.tota_poemb[ll_fila]
//			dw_variedad.Object.tota_poest[ll_fila_b]  = dw_variedad.Object.tota_poest[ll_fila_b] + ids_2.Object.tota_poest[ll_fila]
//			dw_variedad.Object.tota_podif[ll_fila_b]  = dw_variedad.Object.tota_podif[ll_fila_b] + ids_2.Object.tota_podif[ll_fila]
			dw_variedad.Object.tota_poemb[ll_fila_b]  = ld_valor2
			dw_variedad.Object.tota_poest[ll_fila_b]  = ld_valor1
			dw_variedad.Object.tota_podif[ll_fila_b]= ld_valor2 - ld_valor1

			dw_variedad.Object.tota_kgcho[ll_fila_b]  = dw_variedad.Object.tota_kgcho[ll_fila_b] + ids_2.Object.tota_kgcho[ll_fila]
			dw_variedad.Object.tota_cjcho[ll_fila_b]  = dw_variedad.Object.tota_cjcho[ll_fila_b] + ids_2.Object.tota_cjcho[ll_fila]
			dw_variedad.Object.tota_pocho[ll_fila_b]  = dw_variedad.Object.tota_pocho[ll_fila_b] + ids_2.Object.tota_pocho[ll_fila]
			dw_variedad.Object.tota_kgfdn[ll_fila_b]  = dw_variedad.Object.tota_kgfdn[ll_fila_b] + ids_2.Object.tota_kgfdn[ll_fila]
			dw_variedad.Object.tota_cjfdn[ll_fila_b]  = dw_variedad.Object.tota_cjfdn[ll_fila_b] + ids_2.Object.tota_cjfdn[ll_fila]
			dw_variedad.Object.tota_pofdn[ll_fila_b]  = dw_variedad.Object.tota_pofdn[ll_fila_b] + ids_2.Object.tota_pofdn[ll_fila]
			dw_variedad.Object.tota_kgcom[ll_fila_b]  = dw_variedad.Object.tota_kgcom[ll_fila_b] + ids_2.Object.tota_kgcom[ll_fila]
			dw_variedad.Object.tota_cjcom[ll_fila_b]  = dw_variedad.Object.tota_cjcom[ll_fila_b] + ids_2.Object.tota_cjcom[ll_fila]
			dw_variedad.Object.tota_pocom[ll_fila_b]  = dw_variedad.Object.tota_pocom[ll_fila_b] + ids_2.Object.tota_pocom[ll_fila]
		END IF
	NEXT
END IF
RETURN
end subroutine

on w_consulta_sistemaestimacion.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_variedad=create dw_variedad
this.dw_detalle=create dw_detalle
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_variedad
this.Control[iCurrent+3]=this.dw_detalle
end on

on w_consulta_sistemaestimacion.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_variedad)
destroy(this.dw_detalle)
end on

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

istr_anfin	=	Message.PowerObjectParm

dw_2.GetChild("agro_codigo", idwc_Agronomo)
idwc_Agronomo.SetTransObject(SQLCA)
idwc_Agronomo.Retrieve()

dw_2.GetChild("prod_codigo",idwc_Productor)
idwc_Productor.SetTransObject(SQLCA)
idwc_Productor.Retrieve(-1)

dw_2.GetChild("plde_codigo",idwc_Packing)
idwc_Packing.SetTransObject(SQLCA)
idwc_Packing.Retrieve(-1)

dw_2.GetChild("espe_codigo",idwc_Especie)
idwc_Especie.SetTransObject(SQLCA)
idwc_Especie.Retrieve(-1, -1)

dw_2.GetChild("vari_codigo",idwc_variedad)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Variedad.Retrieve(-1, -1, -1)

dw_2.SetTransObject(SQLCA)
dw_2.InsertRow(0)

iuo_productor			=	Create uo_productores
iuo_agroespeprod		=	Create uo_agronomo_productor
iuo_especie				=	Create uo_especie
iuo_variedad			=	Create uo_variedades

ids_1 = CREATE DataStore
ids_2 = CREATE DataStore

If UpperBound(istr_anfin.Argum) > 0 Then
	dw_2.Object.todosagro[1]		=  1
	dw_2.object.todoscoor[1]		=  0
	dw_2.object.agro_coordi[1]		= integer(istr_anfin.Argum[6])
	dw_2.Object.prod_codigo[1] 	= integer(istr_anfin.Argum[2])
	iuo_Productor.Existe(Integer(istr_anfin.Argum[2]),True,SQLCA)
	
	If istr_anfin.Argum[3] <> '' Then
		dw_2.Object.todosvari[1]		=  0
		dw_2.Object.espe_codigo[1] 	= Integer(istr_anfin.Argum[3])
		dw_2.Object.vari_codigo[1] 	= Integer(istr_anfin.Argum[4])
	Else
		dw_2.Object.todosespe[1]		=  1
		dw_2.Object.todosvari[1]		=  1
	End If	

Else
	If gstr_Agro.CodigoAgronomo <>0 and isnull(gstr_agro.CodigoAgronomo) = False Then
		dw_2.Object.agro_codigo[1] 	=	gstr_agro.CodigoAgronomo
		idwc_Productor.Retrieve(gstr_agro.CodigoAgronomo)
		idwc_Especie.Retrieve(gstr_agro.CodigoAgronomo, -1)
	End If
	
	If gstr_agro.Administrador <> 1 Then
		dw_2.Object.agro_codigo.Protect 				=  1
		dw_2.Object.agro_codigo.Color				=	RGB(255,255,255)
		dw_2.Object.agro_codigo.BackGround.Color	=	553648127
		dw_2.Object.todosagro.visible 					= 	0
	Else	
		dw_2.Object.agro_codigo.Protect 				=  0
		dw_2.Object.agro_codigo.Color				=	0
		dw_2.Object.agro_codigo.BackGround.Color	=	RGB(255,255,255)
		dw_2.Object.todosagro.visible 					= 	1
	End If	
End If

dw_1.SetTransObject(sqlca)
dw_1.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.ModIfy("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descEndiente */
buscar	= "Código:Ncodigo,Productor:Nprod_codigo,Predio:Nprbr_codigo,Especie:Nespe_codigo,Variedad:Nvari_codigo"
ordenar	= "Código:codigo,Productor:prod_codigo,Predio:prbr_codigo,Especie:espe_codigo,Variedad:vari_codigo"

DataStore lds_semana
Integer   li_semana
Date      ld_fecha

lds_semana 		= CREATE DataStore

lds_semana.DataObject = 'dw_consulta_numerosemana'
lds_semana.SetTransObject(SQLCA)

lds_semana.Retrieve(li_semana,gstr_tempo.temporada)

If lds_semana.Rowcount() > 0 Then
   li_semana = lds_semana.Object.nume_semana[1] 
	
	dw_2.Object.semana_ini[1] = li_semana
	dw_2.Object.semana_fin[1] = li_semana + 1
End If

ld_fecha = Date(string(today(),'dd/mm/yyyy'))
ld_fecha = Relativedate(ld_fecha,-1)
dw_2.Object.rece_bfecha[1] = Datetime(ld_fecha)

TriggerEvent("ue_recuperadatos")
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	 	ll_fila, respuesta
Integer 	li_agronomo, li_especie, li_variedad, li_temporada, li_productor, li_zona,&
			li_sema1, li_sema2, li_grupoprod, li_coordina, li_planta, li_resumen
Datetime ld_fecha

dw_2.Accepttext()
dw_1.Reset()

IF dw_2.Object.todosagro[1] = 1 THEN
	li_agronomo = -1
ELSE
	li_agronomo = dw_2.Object.agro_codigo[1]
END IF 

IF dw_2.Object.todosespe[1] = 1 THEN
	li_especie = -1
ELSE
	li_especie = dw_2.Object.espe_codigo[1]
END IF

IF dw_2.Object.todosvari[1] = 1 THEN
	li_variedad = -1
ELSE
	li_variedad = dw_2.Object.vari_codigo[1]
END IF

IF dw_2.Object.todosprod[1] = 1 THEN
	li_productor = -1
ELSE
	li_productor = dw_2.Object.prod_codigo[1]
END IF

IF dw_2.Object.todoszona[1] = 1 THEN
	li_zona = -1
ELSE
	li_zona = dw_2.Object.zona_codigo[1]
END IF

IF dw_2.Object.grupo_prod[1] = 1 THEN
	li_productor = -1
	li_grupoprod = iuo_Productor.grupoprod
ELSE
	li_grupoprod = -1
END IF

IF dw_2.Object.todoscoor[1] = 1 THEN
	li_coordina = -1
ELSE
	li_coordina = dw_2.Object.agro_coordi[1]
END IF

IF dw_2.Object.todosplan[1] = 1 THEN
	li_planta = -1
ELSE
	li_planta = dw_2.Object.plde_codigo[1]
END IF

li_sema1 = dw_2.Object.semana_ini[1]
li_sema2 = dw_2.Object.semana_fin[1]
ld_fecha = dw_2.Object.rece_bfecha[1]

li_temporada = gstr_tempo.Temporada
dw_1.SetTransObject(SQLCA)

DO
	IF dw_2.Object.tipo_selecc[1] = 2 THEN
		procesoinfoprod()
	ELSE	
		IF dw_2.Object.tipo_selecc[1] = 4 THEN
			li_resumen = dw_2.Object.sele_resume[1]
			if dw_2.object.semana_cons[1] = 1 then
				li_resumen = 2
			END IF
			ll_fila	= dw_1.Retrieve(li_sema1,li_sema2,li_temporada, li_agronomo, li_productor,-1,li_planta,&
											 li_especie,li_variedad,li_zona, li_grupoprod, li_coordina, li_resumen)
		ELSEIF dw_2.Object.tipo_selecc[1] = 1 THEN
			ll_fila	= RecuperaInfoPlantas(li_temporada, li_agronomo, li_productor, li_especie, &
											 li_variedad, li_zona, li_grupoprod, li_coordina, ld_fecha)
		ELSEIF dw_2.Object.tipo_selecc[1] = 0 THEN
			
			IF dw_2.Object.sele_nordis[1] = 0 OR dw_2.Object.sele_tipopron[1] = 1 THEN
				ll_fila	= dw_1.Retrieve(li_temporada, li_agronomo, li_productor, li_especie, &
												 li_variedad, li_zona, li_grupoprod, li_coordina, &
												 dw_2.Object.sele_atributo[1], dw_2.Object.sele_tipopron[1],&
												 dw_2.Object.sele_cajabin[1], li_planta)
			ELSE
				ll_fila	= dw_1.Retrieve(li_temporada, li_agronomo, li_productor, li_especie, &
												 li_variedad, li_zona, li_grupoprod, li_coordina, &
												 dw_2.Object.sele_atributo[1],li_planta)
			END IF	
		ELSE									 
			ll_fila	= dw_1.Retrieve(li_temporada, li_agronomo, li_productor, li_especie, &
											 li_variedad, li_zona, li_grupoprod, li_coordina,dw_2.Object.sele_atributo[1])
		END IF
		
		IF ll_fila = -1 THEN
			respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
		ELSEIF ll_fila > 0 THEN
			dw_1.SetRow(1)
			dw_1.SetFocus()
			pb_imprimir.Enabled	= True
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;
SetPointer(HourGlass!)

dw_2.Accepttext()

IF dw_2.Object.tipo_selecc[1] = 0 AND dw_2.Object.sele_tipopron[1] = 0 AND &
   dw_2.Object.sele_nordis[1] = 1 THEN
	
	Long		fila
	Integer li_agronomo, li_especie, li_variedad, li_productor, li_zona, li_grupoprod,&
	        li_coordina, li_temporada, li_planta
			  
	IF dw_2.Object.todosagro[1] = 1 THEN
		li_agronomo = -1
	ELSE
		li_agronomo = dw_2.Object.agro_codigo[1]
	END IF 

	IF dw_2.Object.todosespe[1] = 1 THEN
		li_especie = -1
	ELSE
		li_especie = dw_2.Object.espe_codigo[1]
	END IF

	IF dw_2.Object.todosvari[1] = 1 THEN
		li_variedad = -1
	ELSE
		li_variedad = dw_2.Object.vari_codigo[1]
	END IF

	IF dw_2.Object.todosprod[1] = 1 THEN
		li_productor = -1
	ELSE
		li_productor = dw_2.Object.prod_codigo[1]
	END IF

	IF dw_2.Object.todoszona[1] = 1 THEN
		li_zona = -1
	ELSE
		li_zona = dw_2.Object.zona_codigo[1]
	END IF

	IF dw_2.Object.grupo_prod[1] = 1 THEN
		li_productor = -1
		li_grupoprod = iuo_Productor.grupoprod
	ELSE
		li_grupoprod = -1
	END IF

	IF dw_2.Object.todoscoor[1] = 1 THEN
		li_coordina = -1
	ELSE
		li_coordina = dw_2.Object.agro_coordi[1]
	END IF

	IF dw_2.Object.todosplan[1] = 1 THEN
		li_planta = -1
	ELSE
		li_planta = dw_2.Object.plde_codigo[1]
	END IF

	li_temporada = gstr_tempo.Temporada
	
	istr_info.titulo	= "PRONOSTICO DE COSECHA DISTRIBUIDO POR CATEGORIAS"
	istr_info.copias	= 1
	
	OpenWithParm(vinf, istr_info)
	
	vinf.dw_1.DataObject = "dw_info_pronosdistribcatego_composite"
	
	vinf.dw_1.SetTransObject(sqlca)
	
	fila = vinf.dw_1.Retrieve(li_temporada, li_agronomo, li_productor, li_especie, &
									 li_variedad, li_zona, li_grupoprod, li_coordina, &
									 dw_2.Object.sele_atributo[1], li_planta)
	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF

ELSE

	String	ls_titulo[]
	
	ls_titulo[1]	=	'PRONOSTICO DE COSECHA'
	ls_titulo[2]	=	'INFORME DE RECEPCION'
	ls_titulo[3]	=	'INFORME DE PROCESOS'
	ls_titulo[4]	=	'CAJAS EMBALADAS NORMADAS'
	ls_titulo[5]	=	'PROGRAMA DE COSECHA'
	
	str_info	lstr_info
	
	lstr_info.titulo	= ls_titulo[dw_2.Object.tipo_selecc[1]+1]
		
	lstr_info.copias	= 1
	
	OpenWithParm(vinf,lstr_info)
	
	vinf.dw_1.DataObject = "dw_consultas_pronosticocosech_info" //"dw_consulta_programacosechasemana" dw_1.DataObject 
	
	IF dw_2.Object.tipo_selecc[1] = 0 AND dw_2.Object.sele_tipopron[1] = 1 THEN
		vinf.dw_1.SetTransObject(SQLCA)
		vinf.dw_1.Retrieve(gstr_tempo.temporada, -7, -7, -7, -7, -7, -7, -7, &
								 dw_2.Object.sele_atributo[1], dw_2.Object.sele_tipopron[1], &
								 dw_2.Object.sele_cajabin[1])
	END IF
	
	dw_1.ShareData(vinf.dw_1)
	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("DataWindow.Header.Height=250")
//	IF dw_2.Object.tipo_selecc[1] = 4 THEN
//		IF dw_2.Object.sele_resume[1] = 1 THEN
//			vinf.dw_1.Object.DataWindow.Detail.Height = 0
//			vinf.dw_1.Modify("DataWindow.Header.2.Height=0")
//		ELSE
//			dw_1.Object.DataWindow.Detail.Height = 140
//			dw_1.Modify("DataWindow.Header.2.Height=250")
//		END IF
//	END IF			
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
	vinf.Visible	= True
	vinf.Enabled	= True
	
END IF

SetPointer(Arrow!)
end event

event activate;call super::activate;IF gs_graba = 'Grabado' THEN
	gs_graba = ''
	TriggerEvent("ue_recuperadatos")
END IF

IF ii_filadc <> 0 THEN
		dw_1.SetRow(ii_filadc)
		dw_1.ScrollToRow(ii_filadc)
		dw_1.SetFocus()
END IF	
end event

type dw_1 from w_mant_tabla`dw_1 within w_consulta_sistemaestimacion
integer x = 32
integer y = 380
integer width = 4256
integer height = 1260
string dataobject = "dw_consultas_pronosticocosech"
boolean hscrollbar = true
end type

event dw_1::rbuttondown;call super::rbuttondown; m_consulta_sis	l_Menu 
String ls_columna

IF Row =	0	THEN
	Return
ELSE
	IF dw_2.Object.tipo_selecc[1] = 0 AND ((dw_2.Object.sele_tipopron[1] = 0 AND &
	   dw_2.Object.sele_nordis[1] = 0) OR (dw_2.Object.sele_tipopron[1] =  1)) AND &
		dw_2.Object.sele_atributo[1] = 1 THEN
		ls_columna = dwo.name 
		
		gi_tiposel = 0
		
		gstr_us.OpcionActiva	=	Parent.ClassName()
		il_fila 					=	Row
		ii_filadc				=	il_fila
		This.SetRow(il_fila)
				
		l_Menu = CREATE m_consulta_sis
		
		l_Menu.m_m_edicion.m_pronósticocosecha.visible 	= TRUE
		
		l_Menu.m_m_edicion.PopMenu(w_main.PointerX(),w_main.PointerY())
	END IF
	
	IF dw_2.Object.tipo_selecc[1] = 1 THEN
		ls_columna = dwo.name 
		
		gi_tiposel = 0
		
		gstr_us.OpcionActiva	=	Parent.ClassName()
		il_fila 					=	Row
		This.SetRow(il_fila)
				
		l_Menu = CREATE m_consulta_sis
		
		l_Menu.m_m_edicion.m_pronósticocosecha.visible 	= FALSE
		
		l_Menu.m_m_edicion.PopMenu(w_main.PointerX(),w_main.PointerY())
	END IF
	
END IF

end event

event dw_1::doubleclicked;call super::doubleclicked;ii_filadc = 0

IF dw_2.Object.tipo_selecc[1] = 4 and row > 0 AND NOT istr_mant.Solo_Consulta THEN
	SetPointer(HourGlass!)
	
	IF dw_2.Object.sele_resume[1] <> 1 THEN

	END IF
END IF
end event

event dw_1::retrievestart;call super::retrievestart;RETURN 2
end event

type st_encabe from w_mant_tabla`st_encabe within w_consulta_sistemaestimacion
boolean visible = false
integer x = 69
integer y = 28
integer width = 3017
integer height = 316
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_consulta_sistemaestimacion
integer x = 4407
integer y = 52
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_consulta_sistemaestimacion
boolean visible = false
integer x = 4512
integer y = 420
integer height = 248
end type

event pb_nuevo::clicked;call super::clicked;dw_2.Enabled = TRUE
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_consulta_sistemaestimacion
boolean visible = false
integer x = 4512
integer y = 576
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_consulta_sistemaestimacion
boolean visible = false
integer x = 4512
integer y = 756
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_consulta_sistemaestimacion
boolean visible = false
integer x = 4512
integer y = 936
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_consulta_sistemaestimacion
integer x = 4462
integer y = 952
end type

type pb_salir from w_mant_tabla`pb_salir within w_consulta_sistemaestimacion
integer x = 4480
integer y = 1412
end type

type dw_2 from datawindow within w_consulta_sistemaestimacion
integer x = 69
integer y = 24
integer width = 3538
integer height = 304
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_consulta_sistemaestimacion"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
String  ls_columna

SetNull(li_Null)

ls_columna = dwo.name 
 
Choose Case ls_columna
	Case "espe_codigo"	
		If Not iuo_especie.existe(Integer(data),True, SQLCA) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		Else
			This.GetChild("vari_codigo",idwc_Variedad)
			idwc_variedad.SetTransObject(SqlCa)
			idwc_variedad.Retrieve(iuo_Productor.Codigo, -1, Integer(Data))
		End If 

	Case "vari_codigo"	
		If Not iuo_variedad.existe(this.Object.espe_codigo[1],Integer(data),TRUE,SQLCA)  Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If 
			  
	Case "prod_codigo"	
		If Not iuo_Productor.Existe(Long(Data),True,SQLCA) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		Else
			This.GetChild("plde_codigo",idwc_Packing)
			idwc_Packing.SetTransObject(SqlCa)
			idwc_Packing.Retrieve(iuo_Productor.Codigo)
			
			This.GetChild("espe_codigo",idwc_Especie)
			idwc_Especie.SetTransObject(SqlCa)
			
			If This.Object.todosagro[Row] = 1 Then
				idwc_Especie.Retrieve(-1, iuo_Productor.Codigo)
			Else
				idwc_Especie.Retrieve(This.Object.agro_codigo[Row], iuo_Productor.Codigo)
			End If
			
			This.GetChild("vari_codigo",idwc_Variedad)
			idwc_Variedad.SetTransObject(SqlCa)
			idwc_Variedad.Retrieve(iuo_Productor.Codigo, -1, -1)		
		End If
		
	Case 'agro_codigo'
		dw_2.GetChild("prod_codigo",idwc_Productor)
		idwc_Productor.SetTransObject(SQLCA)
		idwc_Productor.Retrieve(Integer(Data))
		
		dw_2.GetChild("espe_codigo",idwc_Especie)
		idwc_Especie.SetTransObject(SQLCA)
		idwc_Especie.Retrieve(Integer(Data), This.Object.prod_codigo[Row])
		
	Case "todosespe"
		If data = '1' Then
			dw_2.SetItem(row,"espe_codigo",li_Null)
			dw_2.SetItem(row,"vari_codigo",li_Null)
		End If
		
	Case "todosvari"
		If data = '1' Then
			dw_2.SetItem(row,"vari_codigo",li_Null)
		End If
	
	Case "todosagro"
		If data = '1' Then 
			dw_2.SetItem(row,"agro_codigo",li_Null)
			dw_2.Object.agro_codigo.Protect	= 1
			dw_2.Object.agro_codigo.Color 	= RGB(255,255,255)
			dw_2.Object.agro_codigo.BackGround.Color = 553648127
			dw_2.GetChild("prod_codigo",idwc_Productor)
			idwc_Productor.SetTransObject(SQLCA)
			idwc_Productor.Retrieve(-1)
			
			dw_2.GetChild("espe_codigo",idwc_Especie)
			idwc_Especie.SetTransObject(SQLCA)
			idwc_Especie.Retrieve(-1, -1)
			
			dw_2.GetChild("vari_codigo",idwc_variedad)
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Variedad.Retrieve(-1, -1, -1)
			
		Else	
			dw_2.Object.agro_codigo.Protect = 0
			dw_2.Object.agro_codigo.Color 	= 0
			dw_2.Object.agro_codigo.BackGround.Color = RGB(255,255,255)
		End If
		
	Case "todosprod"
		If data = '1' Then
			dw_2.SetItem(row,"prod_codigo",li_Null)
		End If	

	Case "todosplan"
		If data = '1' Then
			dw_2.SetItem(row,"plde_codigo",li_Null)
		End If	
End CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()

RETURN 0
end event

event rbuttondown;m_consulta_sis	l_Menu
String ls_columna

IF RowCount() =	0	THEN
	Return
ELSE
	ls_columna = dwo.name 
		
	gi_tiposel = 100
		
	gstr_us.OpcionActiva	=	Parent.ClassName()
	il_fila 					=	Row
	This.SetRow(il_fila)
				
	l_Menu = CREATE m_consulta_sis
	l_Menu.m_m_edicion.PopMenu(w_main.PointerX(),w_main.PointerY())	
END IF
end event

type dw_variedad from uo_dw within w_consulta_sistemaestimacion
boolean visible = false
integer x = 2921
integer y = 1632
integer width = 311
integer height = 176
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_info_resumen_proceso_variedad"
end type

type dw_detalle from uo_dw within w_consulta_sistemaestimacion
boolean visible = false
integer x = 3392
integer y = 1576
integer width = 320
integer height = 192
integer taborder = 21
string dataobject = "dw_info_resumen_proceso_productor"
end type

