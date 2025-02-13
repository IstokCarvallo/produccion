$PBExportHeader$w_info_recepcion_movimiento_diario.srw
$PBExportComments$Informe Cartola de Movimientos Fruta Granel
forward
global type w_info_recepcion_movimiento_diario from w_para_informes
end type
type dw_1 from datawindow within w_info_recepcion_movimiento_diario
end type
end forward

global type w_info_recepcion_movimiento_diario from w_para_informes
string tag = "Informe Recepciones Fruta Comercial"
integer x = 14
integer y = 32
integer width = 2994
integer height = 1728
string title = "Informe Recepciones Fruta Comercial"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_recepcion_movimiento_diario w_info_recepcion_movimiento_diario

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
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto, idwc_tipoenvase, idwc_sentido, &
						idwc_movto, idwc_exporta, idwc_mvto

String		is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio, ii_mvto, ii_sentido
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
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Tipo Envase (" + String(ai_tipo_enva) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE
end function

on w_info_recepcion_movimiento_diario.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_recepcion_movimiento_diario.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio			=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_predios				=	Create uo_predios
iuo_centrocosto		=	Create uo_centrocostos
iuo_camarasfrigo		=  Create uo_camarasfrigo

//Exportador
dw_1.GetChild("cliente", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportador")
	idwc_exporta.InsertRow(0)
END IF

//Movimiento
dw_1.GetChild("movimiento", idwc_mvto)
idwc_mvto.SetTransObject(sqlca)
IF idwc_mvto.Retrieve(Integer(Message.StringParm)) = 0 THEN
	MessageBox("Atención","Falta Registrar Tipos de Movimiento Correspondientes")
	idwc_mvto.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

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
IF idwc_variedad.Retrieve(0,gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
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
IF idwc_productor.Retrieve(-1) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

ii_sentido = integer(Message.StringParm)

IF ii_sentido = 1 THEN
	st_titulo.text	=	'Informe Recepción de Fruta Comercial Diaria'
ELSE
	st_titulo.text	=	'Informe Despacho de Fruta Comercial Diaria'
END IF

Paramtemporada(gstr_paramtempo)

dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.Setitem(1,"cliente",gi_codexport)
end event

type pb_excel from w_para_informes`pb_excel within w_info_recepcion_movimiento_diario
integer y = 628
integer height = 240
end type

type st_computador from w_para_informes`st_computador within w_info_recepcion_movimiento_diario
integer x = 1006
integer width = 1929
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepcion_movimiento_diario
integer x = 1006
integer width = 1929
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepcion_movimiento_diario
integer x = 1006
integer width = 1929
end type

type p_logo from w_para_informes`p_logo within w_info_recepcion_movimiento_diario
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepcion_movimiento_diario
integer width = 2304
string text = "Informe Recepción de Fruta Comercial Diaria"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepcion_movimiento_diario
integer x = 2656
integer y = 536
integer height = 212
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Predio, li_Ccosto, &
			li_Variedad, li_Periodo, li_Categoria, li_tipoenvase,li_movto, &
			li_ConsLote, li_Consfechad, li_sentidomov, li_exporta
Long		ll_Fila, ll_Productor
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento, ls_fecha, ls_grupocalibre

If ii_sentido = 1 Then
	istr_info.titulo	= 'RECEPCIONES DIARIAS FRUTA COMERCIAL'
Else
	istr_info.titulo	= 'DESPACHOS DIARIOS FRUTA COMERCIAL'
End If

OpenWithParm(vinf, istr_info)

If dw_1.Object.envase[1] = 1 Then
	vinf.dw_1.DataObject = "dw_info_recepcion_diaria_nenvase"
Else	
	vinf.dw_1.DataObject = "dw_info_recepcion_frutacomercial_diaria"
End If		

dw_1.accepttext()

//Acepta Exportador
li_exporta = dw_1.Object.cliente[1]

If IsNull(li_exporta) Then
	MessageBox( "Exportador Erróneo", "Falta seleccionar un Exportador.",  StopSign!, Ok!)
	Return 1				 
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = 10000
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If


// Acepta Movimiento //
If dw_1.Object.todosmovto[1] = 1 Then
	ii_mvto = 0
Else
	ii_mvto	= dw_1.Object.movimiento[1]
	If IsNull(ii_mvto) Then
		MessageBox( "Movimiento Erróneo", "Falta seleccionar un Movimiento.", StopSign!, Ok!)
		Return 1				 
   End If
End If	

// Acepta Especie //
If dw_1.Object.todosespe[1] = 1 Then
	li_Especie = 100
Else
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Variedad //
If dw_1.Object.todosvari[1] = 1 Then
	li_Variedad = 10000
Else
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Tratamientos de frio //
If dw_1.Object.todostrata[1] = 1 Then
	ls_Tratamiento = '*'
	If dw_1.Object.constrata[1] = 1 Then	
		ls_Tratamiento = '**'
	End If
Else
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Categorias
If dw_1.Object.todoscate[1] = 1  Then
	li_Categoria = 1000
Else
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Productor
If dw_1.Object.todosprod[1] = 1 Then
	ll_Productor = 10000
	If dw_1.Object.consprod[1] = 1 Then	
		ll_Productor =	90000
	End If
Else
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta GrupoCalibre //
If dw_1.Object.todosgc[1] = 1 Then
	If dw_1.Object.agrupagc[1] = 1 Then
		ls_grupocalibre = '**'
	Else
		ls_GrupoCalibre = '*'
	End If
Else
	ls_GrupoCalibre = dw_1.Object.GrupoCalibre[1]
	If IsNull(ls_GrupoCalibre) Then
		MessageBox( "Grupo Calibre Errónea", "Falta seleccionar una Grupo Calibre.", StopSign!, Ok!)
		Return 1				 
   End If
End If

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	Return 1				 
End If

ls_fecha			=	String(dw_1.Object.fechah[1])
ld_fechahasta = 	Date(ls_fecha)
If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	Return 1				 
End If

If ld_fechadesde > ld_fechahasta Then
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.",StopSign!, Ok!)
	Return 1
End If	

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta,li_Especie,li_Variedad, ls_Tratamiento, ll_Productor, &
									 li_Categoria, ii_sentido, ld_fechadesde, ld_fechahasta, 0, li_exporta,ii_mvto,ls_GrupoCalibre)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)	
	If dw_1.Object.totalespe[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.3.Height=0")
	If dw_1.Object.totalprod[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.4.Height=0")
	If dw_1.Object.totalvari[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.5.Height=0")
	If dw_1.Object.totaltrat[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.6.Height=0")
	vinf.dw_1.Object.DataWindow.Zoom = 90
	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepcion_movimiento_diario
integer y = 824
integer height = 212
integer taborder = 380
end type

type dw_1 from datawindow within w_info_recepcion_movimiento_diario
integer x = 251
integer y = 440
integer width = 2240
integer height = 940
integer taborder = 380
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_recepcion_diaria_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null, li_cliente
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
		END IF		
		
	CASE "especie"
		This.Object.Variedad[row]	=	li_Null
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_variedad.settransobject(sqlca)
			idwc_variedad.Retrieve(Integer(data),gstr_parempresa.empr_codexp)
		END IF
	
  CASE	"todosespe"
		IF	data = '1' THEN
			This.Object.especie[row]		=	li_Null
			this.Object.variedad[row]		=	li_Null
			dw_1.Object.todosvari[row]		=	1
		ELSE
			This.Object.especie[row]		=	li_Null
			This.Object.Variedad[row]		=	li_Null
		END IF	
	

	CASE "variedad"
	
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
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
	
	CASE "Mvto"
		IF idwc_mvto.Find("tpmv_codigo = " + data, 1, idwc_mvto.RowCount()) = 0 THEN
			MessageBox("Error", "El tipo de movimiento seleccionado no existe o no corresponde a la ventana")
			This.SetItem(1, "Mvto", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]		=	String(li_Null)
	
	CASE	"todosMvto"
		IF	data = '1' THEN This.Object.mvto[row]				=	String(li_Null)
		
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]		=	li_Null
			
	CASE	"todosprod"
		IF	data = '1' THEN This.Object.productor[row]		=	li_Null
			
	CASE	"todosgc"
		IF	data = '1' THEN This.Object.GrupoCalibre[row]	=	String(li_Null)
		
	CASE  "consfrigo"
		IF data = '1' THEN
			dw_1.Object.conscamara[row] = 1
		ELSE
			dw_1.Object.conscamara[row] = 0
		END IF
	
END CHOOSE
end event

event itemerror;Return 1
end event

