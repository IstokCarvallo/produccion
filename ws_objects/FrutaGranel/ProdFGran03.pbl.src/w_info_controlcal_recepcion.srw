$PBExportHeader$w_info_controlcal_recepcion.srw
$PBExportComments$Ventana de Informe de Control de Calidad Recepción Distribución Calibres y Color
forward
global type w_info_controlcal_recepcion from w_para_informes
end type
type dw_1 from datawindow within w_info_controlcal_recepcion
end type
end forward

global type w_info_controlcal_recepcion from w_para_informes
integer x = 14
integer y = 32
integer width = 2656
integer height = 1832
string title = "Control de Calidad Recepción [ Distribución Calibres y Color ]"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_controlcal_recepcion w_info_controlcal_recepcion

type variables

uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores			iuo_productores
uo_predios           iuo_predios
uo_centrocostos      iuo_centrocosto

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto, idwc_exporta

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio
end variables

forward prototypes
public function boolean noexistetipoenvase (integer ai_tipo_enva)
end prototypes

public function boolean noexistetipoenvase (integer ai_tipo_enva);String ls_tipo

SELECT	tien_nombre
	INTO	:ls_tipo
	FROM	dba.tiposenvases
	WHERE	enva_tipoen	=	:ai_tipo_enva ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla TiposEnvases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Código de Tipo Envase (" + String(ai_tipo_enva) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE
end function

on w_info_controlcal_recepcion.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_controlcal_recepcion.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_especie				=	Create uo_especie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_predios				=	Create uo_predios
iuo_centrocosto		=	Create uo_centrocostos

//Exportador
dw_1.GetChild("exportador", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Clientes")
	idwc_exporta.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

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

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
END IF

//Periodo
dw_1.GetChild("periodo",idwc_periodo)
idwc_periodo.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Periodos")
	idwc_periodo.InsertRow(0)
END IF

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
IF idwc_productor.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Predio
dw_1.GetChild("predio",idwc_predio)
idwc_predio.SetTransObject(Sqlca)
IF idwc_predio.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Predios")
	idwc_predio.InsertRow(0)
END IF

//Centro Costo
dw_1.GetChild("centrocosto",idwc_ccosto)
idwc_ccosto.SetTransObject(Sqlca)
IF idwc_ccosto.Retrieve(0,0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Centros de Costo")
	idwc_ccosto.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)


Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.SetItem(1,"exportador",gi_codexport)
end event

type st_titulo from w_para_informes`st_titulo within w_info_controlcal_recepcion
string tag = "Informe Control de Calidad"
integer x = 133
integer y = 60
integer width = 2126
string text = "Informe Control de Calidad Recepción [ Distribución Calibres y Color ]"
boolean righttoleft = true
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_controlcal_recepcion
integer x = 2368
integer y = 544
integer height = 140
integer taborder = 300
end type

event pb_acepta::clicked;
SetPointer(Arrow!)
 
long	   ll_Planta,        ll_Especie,  ll_Predio,    ll_Ccosto,     &
			ll_Variedad,      ll_Periodo,  ll_Categoria, ll_tipoenvase, &
			ll_ConsLote
Long		ll_Fila, ll_camara, ll_productor
Integer  li_exporta
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento, ls_fecha


istr_info.titulo	= 'CONTROL DE CALIDAD RECEPCION DISTRIBUCION CALIBRES Y COLOR'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_ccal_recepcion_calibcol_inicio"
dw_1.accepttext()

//Acepta Exportador
li_exporta = dw_1.Object.exportador[1]
IF IsNull(li_exporta) Then
	MessageBox( "Cliente Erróneo", "Falta seleccionar un Cliente.", &
				 StopSign!, Ok!)
	RETURN 1				 
END IF

// Acepta Planta //
IF dw_1.Object.todosplanta[1] = 1 THEN
	ll_Planta = 10000
ELSE
	ll_Planta = dw_1.Object.planta[1]
	IF IsNull(ll_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Camara //
IF dw_1.Object.todascamara[1] = 1 THEN
	ll_Camara = 10000
	IF dw_1.Object.conscama[1] =1 THEN	
		ll_Camara	=	90000
	END IF
ELSE
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Especie //
IF dw_1.Object.todosespe[1] = 1 THEN
	ll_Especie = 100
ELSE
	ll_Especie = dw_1.Object.especie[1]
	If IsNull(ll_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Variedad //
IF dw_1.Object.todosvari[1] = 1 THEN
	ll_Variedad = 10000
ELSE
	ll_Variedad = dw_1.Object.variedad[1]
	If IsNull(ll_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Tratamientos de frio //
IF dw_1.Object.todostrata[1] = 1 THEN
	ls_Tratamiento = '*'
	IF dw_1.Object.constrata[1] = 1 THEN	
		ls_Tratamiento = '**'
	END IF
ELSE
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Periodo de Frio //
IF dw_1.Object.todosperio[1] = 1 THEN
	ll_Periodo = 100
	IF dw_1.Object.consperio[1] = 1 THEN	
		ll_Periodo = 900
	END IF
ELSE
	ll_Periodo = dw_1.Object.periodo[1]
	If IsNull(ll_Periodo) Then
		MessageBox( "Período Erróneo", "Falta seleccionar un Período.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Categorias
IF dw_1.Object.todoscate[1] = 1  THEN
	ll_Categoria = 1000
ELSE
	ll_Categoria = dw_1.Object.categoria[1]
	If IsNull(ll_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todosprod[1] = 1 THEN
	ll_Productor = 10000
	IF dw_1.Object.consprod[1] = 1 THEN	
		ll_Productor =	90000
	END IF
ELSE
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Predio
IF dw_1.Object.todospredio[1] = 1 THEN
	ll_Predio = 100
	IF dw_1.Object.conspredio[1] = 1 THEN	
		ll_Predio =	900
	END IF
ELSE
	ll_Predio = dw_1.Object.predio[1]
	If IsNull(ll_Predio) Then
		MessageBox( "Predio Erróneo", "Falta seleccionar un Predio.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Centro Costo
IF dw_1.Object.todosccosto[1] = 1 THEN
	ll_ccosto = 100
	IF dw_1.Object.conspredio[1] = 1 THEN	
		ll_ccosto =	900
	END IF
ELSE
	ll_ccosto = dw_1.Object.centrocosto[1]
	If IsNull(ll_ccosto) Then
		MessageBox( "Centro Costo Erróneo", "Falta seleccionar un Centro de Costo.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Todos Lotes o Consolida
IF dw_1.Object.consollote[1] = 1 THEN
		ll_ConsLote =	1
	ELSE
		ll_ConsLote = 0
END IF


ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ls_fecha= String(dw_1.Object.fechah[1])
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

IF ld_fechadesde>=ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1
END IF	


vinf.dw_1.SetTransObject(sqlca)

	
ll_Fila	=	vinf.dw_1.Retrieve(ll_Planta, ll_Especie, ll_Categoria, ll_Productor, ll_Variedad, &
                               ll_Camara, ls_Tratamiento, ll_Periodo, ll_predio, ll_ccosto,  &
										 ld_fechadesde, ld_fechahasta, ll_ConsLote, li_exporta)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_controlcal_recepcion
integer x = 2368
integer y = 832
integer height = 140
integer taborder = 310
end type

type dw_1 from datawindow within w_info_controlcal_recepcion
integer x = 133
integer y = 200
integer width = 2117
integer height = 1476
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_ctrlcal_recepcion_seleccion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)

li_cliente = dw_1.Object.exportador[1]

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
			dw_1.Object.todascamara.protect = 0
		END IF		
		
		IF idwc_camaras.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención","Falta Registrar Cámaras Frigoríficas.")
			RETURN 1
		END IF
						
	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.planta[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todascamara.protect = 0
		END IF

	CASE "especie"
		
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_variedad.Retrieve(Integer(data))
			This.Object.Variedad[row]	=	li_Null
		END IF

	CASE "variedad"
		
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "tratamiento"
		IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "periodo"
		IF NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) THEN
			This.SetItem(1, "periodo", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "categoria"
		IF NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "todosplanta"
		IF data='0' THEN 	dw_1.Object.todascamara.protect = 0

		IF data = '1' THEN 
			This.Object.planta[row]		=	li_Null
			dw_1.Object.camara[row]		=	li_Null
			dw_1.Object.todascamara[row]	=	1
		END IF

	CASE "todascamara"
		IF data='0' THEN 
			dw_1.Object.todascamara.protect = 1
		ELSE
			IF data = '1' THEN dw_1.Object.camara[row]	=	li_Null
			dw_1.Object.todascamara[row]	=	1
		END IF
		
	CASE	"todosespe"
		IF	data = '1' THEN This.Object.especie[row]	=	li_Null
			dw_1.Object.variedad[row]	= li_Null
			dw_1.Object.todosvari[row]	=	1

	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[row]	=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
			
	CASE	"todosprod"
		IF	data = '1' THEN This.Object.productor[row]	=	li_Null

	CASE "predio"
		IF NOT iuo_Predios.Existe(SqlCa,True,dw_1.Object.productor[1],Integer(data)) THEN
			This.SetItem(1, "predio", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			this.Object.todosccosto.protect = 	0
			This.Object.centrocosto[row]	  =	li_Null
			IF idwc_ccosto.retrieve(dw_1.Object.productor[1],integer(data)) = 0 THEN
			   MessageBox("Atención","Falta Registrar Predios asociados al Productor-Predio")
			   RETURN 1
		   END IF
			
		END IF
 
   CASE "centrocosto"
		IF NOT iuo_centrocosto.Existe(Sqlca,True,dw_1.Object.productor[1],dw_1.Object.predio[1],Integer(data)) THEN
			This.SetItem(1, "centrocosto", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
	
	CASE	"todospredio"
		IF data='0' THEN
			dw_1.Object.todosccosto.protect = 1
		ELSEIF	data = '1' THEN 
			This.Object.predio[row]			=	li_Null
			This.Object.centrocosto[row]	=	li_Null
		END IF	
	
	CASE	"todosccosto"
		IF	data = '1' THEN This.Object.centrocosto[row]	=	li_Null
	
END CHOOSE
end event

