$PBExportHeader$w_info_tiempo_desverdizado.srw
$PBExportComments$Informe Cartola de Movimientos Fruta Granel
forward
global type w_info_tiempo_desverdizado from w_para_informes
end type
type dw_1 from datawindow within w_info_tiempo_desverdizado
end type
end forward

global type w_info_tiempo_desverdizado from w_para_informes
string tag = "Tiempo en Cámara de Tratamiento"
integer x = 14
integer y = 32
integer width = 2565
integer height = 1628
string title = "Tiempo en Cámara de Tratamiento"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_tiempo_desverdizado w_info_tiempo_desverdizado

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
uo_productores		iuo_productores
uo_predios           		iuo_predios
uo_centrocostos      	iuo_centrocosto

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto, idwc_tipoenvase, idwc_sentido, &
						idwc_movto, idwc_exporta, idwc_variedades

String					is_NomPlanta
Integer				ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio, ii_recepciones, ii_cliente, ii_variedad, ii_camara
Long 					il_tarja, ii_lote
str_mant				istr_mant
end variables

forward prototypes
public subroutine buscalote ()
public subroutine buscatarja ()
public function boolean existelote (integer ai_lote)
public function boolean existetarja (long al_tarja)
end prototypes

public subroutine buscalote ();String  ls_Lote, ls_Null
Str_busqueda	lstr_busq

SetNull(ls_Null)

lstr_busq.Argum[1]	= String(dw_1.Object.planta[1])
lstr_busq.Argum[2]	= String(dw_1.Object.especie[1])
lstr_busq.Argum[3]	= "Consulta"

OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("lote")
	dw_1.SetFocus()
ELSE

	dw_1.Object.planta[1]	=	Integer(lstr_busq.argum[1])
	dw_1.Object.especie[1]	=	Integer(lstr_busq.argum[2])
	dw_1.Object.lote[1]	=	Integer(lstr_busq.argum[3])
	
	Istr_Mant.Argumento[1]	=	lstr_busq.argum[1]
	Istr_Mant.Argumento[2]	=	lstr_busq.argum[2]
	Istr_Mant.Argumento[3]	=	lstr_busq.argum[3]

	Istr_Mant.Argumento[4]			=	String(dw_1.Object.planta[1],"0000") + &
												String(dw_1.Object.especie[1],"00") + &
												String(dw_1.Object.lote[1],"0000")

//	dw_1.Object.lote[1] = Istr_Mant.Argumento[4]
	//TriggerEvent("ue_recuperadatos")
	dw_1.SetColumn("lote")
	dw_1.SetFocus()
END IF

RETURN
end subroutine

public subroutine buscatarja ();String  ls_Lote, ls_Null
Str_busqueda	lstr_busq

SetNull(ls_Null)

lstr_busq.Argum[1]	= String(gstr_Paramplanta.codigoplanta)
lstr_busq.Argum[2]	= ""
lstr_busq.Argum[3]	= "Consulta"

OpenWithParm(w_busc_spro_movtobins, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("lote")
	dw_1.SetFocus()
ELSE

	dw_1.Object.planta[1]		=	Integer(lstr_busq.argum[1])
	dw_1.Object.especie[1]		=	Integer(lstr_busq.argum[2])
	dw_1.Object.lote[1]			=	Integer(lstr_busq.argum[3])
	dw_1.Object.tarja[1] 			= 	Long(lstr_busq.argum[4])
	
	Istr_Mant.Argumento[1]		=	lstr_busq.argum[1]
	Istr_Mant.Argumento[2]		=	lstr_busq.argum[2]
	Istr_Mant.Argumento[3]		=	lstr_busq.argum[3]
	dw_1.Object.TodoLote[1] 	= 	0
	dw_1.Object.TodoTarja[1] 	= 	0
	
	dw_1.SetColumn("tarja")
	dw_1.SetFocus()
END IF

RETURN
end subroutine

public function boolean existelote (integer ai_lote);integer li_cantidad, li_planta, li_especie
Boolean lb_retorno

lb_retorno = True

li_planta 		=	 dw_1.Object.planta[1]
li_especie	=	 dw_1.Object.especie[1]


select Count(*)
into :li_cantidad
from dbo.spro_lotesfrutagranel
where lote_pltcod 	= :li_planta
	and lote_espcod 	= :li_especie
	and lote_codigo 	= :ai_lote;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Lote No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF	

RETURN lb_retorno
end function

public function boolean existetarja (long al_tarja);integer li_cantidad, li_cliente, li_planta
Boolean lb_Retorno

li_cliente		=	dw_1.Object.cliente[1]
li_planta 		= 	dw_1.Object.planta[1]

lb_Retorno	=	True

select count(*)
into :li_cantidad
from dbo.spro_movtobins
where clie_codigo 	= 	:li_cliente
	and plde_codigo 	= 	:li_planta
	and fgmb_nrotar 	=	:al_tarja;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimientos de Bins")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Tarja No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF	

RETURN lb_retorno
end function

on w_info_tiempo_desverdizado.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_tiempo_desverdizado.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_predios				=	Create uo_predios
iuo_centrocosto		=	Create uo_centrocostos
iuo_camarasfrigo		=  Create uo_camarasfrigo

//rb_1.Checked = True
//rb_1.TriggerEvent("Clicked")

//Exportador
dw_1.GetChild("cliente", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportador")
	idwc_exporta.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
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
idwc_camaras.Retrieve(gstr_ParamPlanta.CodigoPlanta, -9)
//idwc_camaras.InsertRow(0)

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
IF idwc_variedad.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
END IF

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
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

//Movimiento
dw_1.GetChild("Movimiento", idwc_movto)
idwc_movto.SetTransObject(sqlca)
IF idwc_movto.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Movimientos")
	idwc_Movto.InsertRow(0)
ELSE
	idwc_movto.SetFilter("tpmv_tipcor = 1" + " and tpmv_frugra = 1")
	idwc_movto.Filter()
END IF


//Variedad
dw_1.GetChild("variedad", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.InsertRow(0)


dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechadesde", RelativeDate(Today(), -31))
dw_1.SetItem(1,"fechahasta", Today())
dw_1.Setitem(1,"cliente",gi_codexport)
//dw_1.Setitem(1,"especie",gstr_ParamPlanta.CodigoEspecie)
//dw_1.SetItem(1, "planta",gstr_ParamPlanta.CodigoPlanta )

ii_cliente = 	gi_codexport
ii_planta	=	-1
end event

type pb_excel from w_para_informes`pb_excel within w_info_tiempo_desverdizado
end type

type st_computador from w_para_informes`st_computador within w_info_tiempo_desverdizado
end type

type st_usuario from w_para_informes`st_usuario within w_info_tiempo_desverdizado
end type

type st_temporada from w_para_informes`st_temporada within w_info_tiempo_desverdizado
end type

type p_logo from w_para_informes`p_logo within w_info_tiempo_desverdizado
end type

type st_titulo from w_para_informes`st_titulo within w_info_tiempo_desverdizado
integer x = 247
integer width = 1737
string text = "Tiempo de Tratamiento en Cámara"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_tiempo_desverdizado
integer x = 2126
integer y = 728
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Predio, li_Ccosto, &
			li_Variedad, li_Periodo, li_Categoria, li_tipoenvase, li_camara, li_tipofrio, &
			li_ConsLote, li_Consfechad, li_sentidomov, li_movto, li_exporta, li_Productor,li_lote
Long		ll_Fila, ll_Frigo, ll_Camara
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento, ls_fecha


OpenWithParm(vinf, istr_info)

IF dw_1.object.inftarja[1] = 1 THEN
	vinf.dw_1.DataObject = "dw_info_tiempo_desverdizado_detalle"
	istr_info.titulo	= "MOVIMIENTOS EN CAMARAS CON TRATAMIENTO NORMAL"
ELSE
	IF dw_1.object.valorizado[1] = 1 THEN
		IF dw_1.object.CobroBins[1] = 0 THEN
			vinf.dw_1.DataObject = "dw_info_tiempo_desverdizado_valorizado"
			istr_info.titulo	= "MOVIMIENTOS EN CAMARAS CON TRATAMIENTO ESPECIAL VALORIZADO"
		ELSE
			vinf.dw_1.DataObject = "dw_info_tiempo_desverdizado_bins_valorizado"
			istr_info.titulo	= "MOVIMIENTOS EN CAMARAS CON COBRO AL BINS"
		END IF
	ELSE
		vinf.dw_1.DataObject = "dw_info_tiempo_desverdizado"
		istr_info.titulo	= "MOVIMIENTOS EN CAMARAS CON TRATAMIENTO ESPECIAL"
	END IF
END IF

dw_1.accepttext()

// Acepta Planta //
IF dw_1.Object.todoplanta[1] = 1 THEN
	li_Planta = -1
ELSE
	li_Planta = dw_1.Object.planta[1]
	IF IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Camara
IF dw_1.Object.todocamara[1] = 1 THEN
	li_Camara 	= 	-1
	li_tipofrio		= 	dw_1.Object.frio[1]
ELSE
	li_Camara 	= 	dw_1.Object.camara[1]
	li_tipofrio	  	=	-1
	IF IsNull(li_Camara) Then
		MessageBox( "Cámara Errónea", "Falta seleccionar una Cámara.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Especie //
IF dw_1.Object.todoespecie[1] = 1 THEN
	li_Especie = -1
ELSE
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todoproductor[1] = 1 THEN
	li_Productor = -1
ELSE
	li_Productor = dw_1.Object.productor[1]
	If IsNull(li_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Lote
IF dw_1.Object.todoLote[1] = 1 THEN
	li_Lote = -1
ELSE
	li_lote = dw_1.Object.lote[1]
	If IsNull(li_lote) Then
		MessageBox( "Lote Erróneo", "Falta Ingresar un número de lote correcto.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta variedad
IF dw_1.Object.todoVariedad[1] = 1 THEN
	li_variedad	= 	-1
ELSE
	li_variedad 	= 	dw_1.Object.variedad[1]
	If IsNull(li_variedad) Then
		MessageBox( "Variedad Erróneo", "Falta Ingresar una Variedad correcta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

ld_fechadesde = dw_1.Object.fechadesde[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ld_fechahasta = 	dw_1.Object.fechahasta[1]
If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

IF ld_fechadesde > ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1
END IF	

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(ii_planta,li_productor,li_especie, li_lote, li_variedad, li_camara, ii_cliente, ld_FechaDesde, ld_FechaHasta, li_tipofrio)

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

type pb_salir from w_para_informes`pb_salir within w_info_tiempo_desverdizado
integer x = 2126
integer y = 1080
integer taborder = 380
end type

type dw_1 from datawindow within w_info_tiempo_desverdizado
integer x = 242
integer y = 388
integer width = 1751
integer height = 1020
integer taborder = 380
string title = "none"
string dataobject = "dw_mant_tiempo_desverdizado"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)
li_cliente = dw_1.Object.cliente[1]
ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoplanta.protect = 0
			ii_planta = Integer(data)
			
			dw_1.GetChild("camara", idwc_camaras)
			idwc_camaras.SetTransObject(sqlca)
			idwc_camaras.Retrieve(ii_planta)
			
			This.SetItem(1, "lote", li_null)
		END IF		
	
	CASE "camara"
		IF NOT iuo_camarasfrigo.Existe(This.Object.planta[row], Integer(data),TRUE, SQLCA ) THEN
			This.SetItem(1, "camara", li_Null 	)
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todocamara.protect = 0
			ii_camara = Integer(data)			
		END IF
		
	CASE "cliente"
		ii_cliente = Integer(data)		
		
	CASE "especie"
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			ii_especie = Integer(data)
			idwc_variedad.SetTransObject(sqlca)
			idwc_variedad.Retrieve(Integer(data))
		END IF
	
	CASE	"todoespecie"
		IF	data = '1' THEN
			This.Object.especie[row]		=	li_Null
			ii_especie = -1
		END IF
		
	CASE "variedad"
		IF NOT iuo_variedades.Existe(This.Object.especie[row], Integer(data), true, sqlca) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			ii_variedad	= 	Integer(data)	
		END IF
		
	CASE "todovariedad"
		IF	data = '1' THEN
			This.Object.variedad[row]		=	li_Null
			ii_variedad = -1
		END IF
		
	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			ii_productor = Integer(data)
		END IF
		
	CASE "todoplanta"
		IF data = '1' THEN 
			This.Object.planta[row]		=	li_Null
			ii_planta = -1
		END IF

	CASE	"todoproductor"
		IF	data = '1' THEN 
			This.Object.productor[row]		=	li_Null
			ii_productor = -1
		END IF
		
	CASE "todolote"
		IF	data = '1' THEN 
			This.Object.lote[row]		=	li_Null
			ii_lote = -1
		END IF
		
	CASE "todotarja"
		IF	data = '1' THEN 
			This.Object.tarja[row]		=	li_Null
			ii_lote = -1
		END IF
	
	CASE "todocamara"
		IF	data = '1' THEN 
			This.Object.camara[row]		=	li_Null
			ii_camara = -1
		END IF
		
	CASE "lote"
		IF ExisteLote(Integer(data)) THEN
			ii_lote = Integer(data)
		ELSE
			SetNull(ii_lote)
			dw_1.SetItem(1, "lote", ii_lote)
			Return -1
		END IF

	CASE "tarja"
		IF ExisteTarja(Long(data)) THEN
			il_tarja = integer(data)
		ELSE
			SetNull(il_tarja)
			dw_1.SetItem(1, "lote", il_tarja)
			Return -1
		END IF
	CASE "valorizado"
		IF data = '1' THEN
			This.Object.inflotes[row] 	= 	1
			This.Object.inftarja[row] 	= 	0
		ELSE
			This.Object.CobroBins[row]	=	0
		END IF
		
	CASE "inflotes"
		This.object.inftarja[row] 		= 0
		This.object.cobrobins[row] 	= 0
		
	CASE "inftarja"
		This.object.inflotes[row] 		= 0
		This.object.cobrobins[row] 	= 0
		
	CASE "cobrobins"
		IF data = '1' THEN
			This.Object.inflotes[row] 	= 	0
		ELSE
			This.object.inflotes[row] 	= 1
		END IF
END CHOOSE
end event

event itemerror;Return 1
end event

event buttonclicked;String ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "b_lote"
		buscalote()
		This.object.todoproductor[row] = 1
		
END CHOOSE
end event

