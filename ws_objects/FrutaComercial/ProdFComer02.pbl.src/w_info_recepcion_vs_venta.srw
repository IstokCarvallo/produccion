$PBExportHeader$w_info_recepcion_vs_venta.srw
$PBExportComments$Ventana de Informe de numerales
forward
global type w_info_recepcion_vs_venta from w_para_informes
end type
type dw_1 from datawindow within w_info_recepcion_vs_venta
end type
end forward

global type w_info_recepcion_vs_venta from w_para_informes
integer x = 14
integer y = 32
integer width = 2633
integer height = 1256
string title = "Informe Venta vs Recepción"
string icon = "\Desarrollo\Produccion_3-2\ProdFrutaGranel\ProdFGranel.ico"
dw_1 dw_1
end type
global w_info_recepcion_vs_venta w_info_recepcion_vs_venta

type variables

uo_plantadesp				iuo_plantadesp
uo_camarasfrigo			iuo_camarasfrigo
uo_especie					iuo_especie
uo_variedades				iuo_variedades
uo_tratamientofrio			iuo_tratamientofrio
uo_categorias				iuo_categorias
uo_productores				iuo_productores
uo_exportadores		   iuo_exportadores
uo_spro_serviciosplanta iuo_servicio
uo_recibidores				iuo_recibidor
uo_clientesprod         	iuo_clientesprod

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_variedad, idwc_tratamiento, idwc_periodo, idwc_productor, idwc_exporta, &
						idwc_servicio
						
String 				is_exportador, is_planta, is_atmosfera, is_especie, is_categoria, is_frigorifico, &
						is_camara, is_variedad, is_productor, is_tipomodulo, is_servicio, is_recibidor
Integer				ii_constarj
end variables

on w_info_recepcion_vs_venta.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_recepcion_vs_venta.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_exportadores		=  Create uo_exportadores
iuo_servicio        		 =  Create uo_spro_serviciosplanta
iuo_recibidor      	  	=  Create uo_recibidores
iuo_camarasfrigo		= 	Create uo_camarasfrigo		
iuo_clientesprod     	=  Create uo_clientesprod

is_tipomodulo			=	Message.StringParm

//Exportador
dw_1.GetChild("exportador", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exporta")
	idwc_exporta.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

//Frigorifico
dw_1.GetChild("frigo", idwc_frigorifico)
idwc_frigorifico.SetTransObject(sqlca)
idwc_frigorifico.InsertRow(0)

//Camara Frigorifico
dw_1.GetChild("camara", idwc_camaras)
idwc_camaras.SetTransObject(sqlca)
idwc_camaras.InsertRow(0)


//Especie
dw_1.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

//Variedad
dw_1.GetChild("variedad",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)
//IF idwc_variedad.Retrieve(0,gstr_parempresa.empr_codexp) = 0 THEN
//	//MessageBox("Atención","Falta Registrar Variedades")
//	idwc_variedad.InsertRow(0)
//END IF

//Categorias
dw_1.GetChild("categoria",idwc_categorias)
idwc_categorias.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Categorias")
	idwc_categorias.InsertRow(0)
END IF
idwc_categorias.Setfilter('isNull( cate_embala ) or  cate_embala <> 1')
idwc_categorias.Filter()

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Servicio
dw_1.GetChild("servicio", idwc_servicio)
idwc_servicio.SetTransObject(sqlca)
IF idwc_servicio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Servicios")
	idwc_servicio.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", Date(gstr_paramtempo.fechainicio))
dw_1.SetItem(1,"fechah", Today())
dw_1.setitem(1,"exportador", gi_codexport)
dw_1.setitem(1,"planta", gstr_paramplanta.codigoplanta)


IF iuo_clientesprod.Existe(gi_codexport,True,SqlCa) = True THEN
	is_Exportador = iuo_clientesprod.Nombre
END IF		

IF iuo_PlantaDesp.Existe(gstr_paramplanta.codigoplanta,True,Sqlca) THEN
	is_planta = iuo_PlantaDesp.nombre
END IF

//GetChild Frigorifico
IF idwc_frigorifico.Retrieve(gstr_paramplanta.codigoplanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar las camaras")
	idwc_frigorifico.InsertRow(0)
ELSE
	idwc_frigorifico.SetFilter("cama_codigo = cama_codfri")
	idwc_frigorifico.Filter()
END IF

//GetChild Camara
IF idwc_camaras.Retrieve(gstr_paramplanta.codigoplanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Cámaras Frigorífico Seleccionado")
	idwc_camaras.InsertRow(0)
END IF

ii_constarj = 1
dw_1.Object.constarj[1] = ii_constarj
end event

type pb_excel from w_para_informes`pb_excel within w_info_recepcion_vs_venta
end type

type st_computador from w_para_informes`st_computador within w_info_recepcion_vs_venta
integer x = 1006
integer width = 1541
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepcion_vs_venta
integer x = 1006
integer width = 1541
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepcion_vs_venta
integer x = 1006
integer width = 1541
end type

type p_logo from w_para_informes`p_logo within w_info_recepcion_vs_venta
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepcion_vs_venta
integer width = 1888
string text = "Informe Ventas Contra Recepciones"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepcion_vs_venta
integer x = 2254
integer y = 484
integer height = 208
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Variedad, li_Categoria, &
			li_ConsProd, li_exporta, li_bultos, li_diasgra, li_Productor, li_servicio, &
			li_resumen, li_recibidor
Long		ll_Fila, ll_Frigo, ll_Camara
Dec{4}  ld_valor_1, ld_valor_2, ld_valor_3, ld_valor_4, ld_valor_5
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba, ld_fecha_1, ld_fecha_2, &
			ld_fecha_3, ld_fecha_4, ld_fecha_5
String	ls_fecha, ls_trata, ls_bultos, ls_diasgra

istr_info.titulo	= 'Informe de Ventas Contra Recepciones'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_recepcion_vs_venta"
vinf.dw_1.SetTransObject(sqlca)

dw_1.accepttext()

// Acepta Exportador
li_exporta = dw_1.Object.exportador[1]
IF IsNull(li_exporta) Then
	MessageBox( "Exportador Erróneo", "Falta seleccionar un Exportador.", StopSign!, Ok!)
	RETURN				 
END IF

// Acepta Planta //
li_Planta = dw_1.Object.planta[1]
IF IsNull(li_Planta) Then
	MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
	RETURN				 
END IF

// Acepta Especie //
IF dw_1.Object.todosespe[1] = 1 THEN
	li_especie 	= 	-1
	is_especie 	= 	'Todas'
ELSE
	li_Especie = dw_1.Object.especie[1]
	IF IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", StopSign!, Ok!)
		RETURN
	END IF
END IF

// Acepta Variedad
IF dw_1.Object.todosvari[1] = 1 THEN
	li_Variedad 	= 	-1
	is_variedad 	=	 'Todos'	
ELSE
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		RETURN				 
   END If
END IF

ld_fechaprueba	= 	date('1900-01-01')
ld_fechadesde 		= 	dw_1.Object.fechad[1]
ld_fechahasta 		=	dw_1.Object.fechah[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	RETURN			 
END If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	RETURN				 
END If

IF ld_fechadesde>=ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor a la Fecha Desde.", StopSign!, Ok!)
	RETURN
END IF

ll_Fila	=	vinf.dw_1.Retrieve(li_exporta, li_especie, li_Variedad, li_Planta, ld_fechadesde, ld_fechahasta, ii_constarj)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)		
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepcion_vs_venta
integer x = 2254
integer y = 772
integer height = 208
integer taborder = 380
end type

type dw_1 from datawindow within w_info_recepcion_vs_venta
integer x = 251
integer y = 440
integer width = 1810
integer height = 548
integer taborder = 60
string title = "none"
string dataobject = "dw_info_sele_recepcion_vs_venta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;//call super::itemchanged;
Integer	li_Null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name
this.accepttext()
CHOOSE CASE ls_Columna
		
	CASE "exportador"
		IF iuo_clientesprod.Existe(Integer(Data),True,SqlCa) = True THEN
			is_Exportador = iuo_clientesprod.Nombre
		ELSE
			This.SetItem(1, "exportador", li_Null )
			This.SetFocus()
			RETURN 1
		END IF	
		
		
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			is_planta = iuo_PlantaDesp.nombre
		END IF		
		
	CASE "frigo"
		IF Not iuo_Camarasfrigo.Existe(dw_1.Object.Planta[1], Integer(data), True, Sqlca) THEN
			This.SetItem(1, "frigo", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoscamara.protect = 0
			is_frigorifico = iuo_Camarasfrigo.nombre
		END IF		

		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()

	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.planta[1],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todoscamara.protect = 0
			is_camara = iuo_Camarasfrigo.nombre
		END IF		
	
	CASE "servicio"
		IF NOT iuo_servicio.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "servicio", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todoserv.protect = 0
			is_servicio = iuo_servicio.nombre
		END IF		
	
	CASE "todosfrigo"
		IF data = '0' THEN
			dw_1.Object.todoscamara.protect = 0
		ELSEIF data = '1' THEN
			dw_1.Object.todoscamara.protect = 1
			This.Object.frigo[row]	= li_Null
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscamara[row]		= 1
		END IF
 
	CASE "todoscama"
		//IF data='1' THEN 
			dw_1.Object.camara[1]	=	li_Null
		//END IF
	
	CASE "especie"
		This.Object.Variedad[row]	=	li_Null
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_variedad.settransobject(sqlca)
			idwc_variedad.Retrieve(Integer(data),gstr_parempresa.empr_codexp)
			is_especie = iuo_especie.nombre
		END IF
	
	CASE	"todosespe"
		IF	data = '1' THEN
			dw_1.Object.especie[row]		=	li_Null
			dw_1.Object.todosvari[row]		=	1
			dw_1.Object.variedad[row]		=	li_Null
		END IF

	CASE "variedad"
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			is_variedad = iuo_Variedades.nombrevariedad
		END IF
		
	CASE	"todosvari"
		IF	data = '1' THEN
			dw_1.Object.variedad[row]		=	li_Null
		END IF
		
	CASE "tratamiento"
		IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			is_atmosfera = iuo_tratamientoFrio.is_frio_nombre
		END IF

	CASE "categoria"
		IF NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			is_categoria = iuo_Categorias.nombre
		END IF

	CASE "productor"
		IF NOT iuo_Productores.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			is_productor = iuo_Productores.nombre
		END IF

	
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todoserv"
		IF	data = '1' THEN This.Object.servicio[row]	=	li_Null
	
	CASE	"constarj"
		ii_constarj = Integer(data)
			
	CASE	"todosprod"
		IF	data = '1' THEN 
			This.Object.productor[row]		=	li_Null
		END IF
				
	CASE "lotesdespa"
		IF data = "1" AND is_tipomodulo="2" THEN
			dw_1.Object.t_recibidor.visible = 1
			dw_1.Object.recibidor.visible   = 1
		ELSE
			dw_1.Object.t_recibidor.visible = 0
			dw_1.Object.recibidor.visible   = 0
		END IF
		
	CASE "recibidor"
		IF iuo_recibidor.existe(Integer(data),True,SQLCA) THEN
			is_recibidor = iuo_recibidor.Nombre
		ELSE
			This.SetItem(1, "recibidor", li_Null )
			This.SetFocus()
			RETURN 1
		END IF			
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

