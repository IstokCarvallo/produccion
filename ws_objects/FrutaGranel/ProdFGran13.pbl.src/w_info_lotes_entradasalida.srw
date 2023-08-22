$PBExportHeader$w_info_lotes_entradasalida.srw
$PBExportComments$Informe Cartola de Movimientos Fruta Granel
forward
global type w_info_lotes_entradasalida from w_para_informes
end type
type dw_1 from datawindow within w_info_lotes_entradasalida
end type
end forward

global type w_info_lotes_entradasalida from w_para_informes
string tag = "Informe Movimientos Lotes Fruta Granel"
integer x = 14
integer y = 32
integer width = 2487
integer height = 1364
string title = "Informe Movimientos Lotes Fruta Granel"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_lotes_entradasalida w_info_lotes_entradasalida

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio		iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores		iuo_productores
uo_predios           		iuo_predios
uo_lotesfrutagranel	iuo_lotes
uo_centrocostos      	iuo_centrocosto

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto, idwc_tipoenvase, idwc_sentido, &
						idwc_movto, idwc_exporta

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio, ii_recepciones, ii_lote, ii_cliente

str_mant				istr_mant
end variables

forward prototypes
public subroutine buscalote ()
end prototypes

public subroutine buscalote ();String  ls_Lote, ls_Null
Str_busqueda	lstr_busq

SetNull(ls_Null)

lstr_busq.Argum[1]	= String(gstr_Paramplanta.codigoplanta)
lstr_busq.Argum[2]	= ""
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

on w_info_lotes_entradasalida.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_lotes_entradasalida.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;Integer	li_Null
x	=	0
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
iuo_camarasfrigo		= 	Create uo_camarasfrigo
iuo_lotes					= 	Create uo_lotesfrutagranel

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
IF idwc_productor.Retrieve(-1) = 0 THEN
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

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

ParamTemporada(gstr_paramtempo)

dw_1.SetItem(1,"fechadesde", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechahasta", Today())
dw_1.Setitem(1,"cliente",gi_codexport)
//dw_1.Setitem(1,"especie",gstr_ParamPlanta.CodigoEspecie)
dw_1.SetItem(1, "planta",gstr_ParamPlanta.CodigoPlanta )


ii_planta 		= 	gstr_ParamPlanta.CodigoPlanta
ii_cliente 	= 	gi_codexport
ii_especie 	=	gstr_ParamPlanta.CodigoEspecie
end event

type pb_excel from w_para_informes`pb_excel within w_info_lotes_entradasalida
end type

type st_computador from w_para_informes`st_computador within w_info_lotes_entradasalida
end type

type st_usuario from w_para_informes`st_usuario within w_info_lotes_entradasalida
end type

type st_temporada from w_para_informes`st_temporada within w_info_lotes_entradasalida
end type

type p_logo from w_para_informes`p_logo within w_info_lotes_entradasalida
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_lotes_entradasalida
integer x = 247
integer width = 1682
string text = "Informe Movimientos Lotes Fruta Granel"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_lotes_entradasalida
integer x = 2057
integer y = 448
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Predio, li_Ccosto, &
			li_Variedad, li_Periodo, li_Categoria, li_tipoenvase, &
			li_ConsLote, li_Consfechad, li_sentidomov, li_movto, li_exporta, li_Productor,li_lote
Long		ll_Fila, ll_Frigo, ll_Camara
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String		ls_Tratamiento, ls_fecha
DataWindowChild ldwc_Productor

istr_info.titulo	= "MOVIMIENTOS DE LOTES"
OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_lotes_movimientos"
dw_1.AcceptText()

// Acepta Planta //
If dw_1.Object.todoplanta[1] = 1 Then
	li_Planta = 0
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Especie //
If dw_1.Object.todoespecie[1] = 1 Then
	li_Especie = 0
Else
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Productor

If dw_1.Object.todoproductor[1] = 1 Then
	li_Productor = 0
Else
	li_Productor = dw_1.Object.productor[1]
	If IsNull(li_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Lote
If dw_1.Object.todoLote[1] = 1 Then
	li_Lote = 0
Else
	li_lote = dw_1.Object.lote[1]
	If IsNull(li_lote) Then
		MessageBox( "Lote Erróneo", "Falta Ingresar un número de lote correcto.", StopSign!, Ok!)
		Return 1				 
   End If
End If

ld_fechadesde = dw_1.Object.fechadesde[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	Return 1				 
End If

ld_fechahasta = 	dw_1.Object.fechahasta[1]
If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	Return 1				 
End If

If ld_fechadesde > ld_fechahasta Then
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", StopSign!, Ok!)
	Return 1
End If


vinf.dw_1.GetChild("prod_codigo", ldwc_productor)
ldwc_productor.SetTransObject(Sqlca)
ldwc_productor.Retrieve(-1)

vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, li_Especie, li_Productor, ld_fechadesde, ld_fechahasta, ii_cliente, li_lote)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_lotes_entradasalida
integer x = 2057
integer y = 732
integer taborder = 380
end type

type dw_1 from datawindow within w_info_lotes_entradasalida
integer x = 238
integer y = 436
integer width = 1682
integer height = 608
integer taborder = 380
string dataobject = "dw_seleccion_lotes_entradasalida"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)
li_cliente = dw_1.Object.cliente[1]
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "planta"
		If NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			Return -1
		Else
			dw_1.Object.todoplanta.protect = 0
			ii_planta = Integer(data)
		End If

	Case "cliente"
		ii_cliente = Integer(data)
		
	Case "especie"
		If NOT iuo_Especie.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			Return -1
		Else
			ii_especie = Integer(data)
			idwc_variedad.settransobject(sqlca)
			idwc_variedad.Retrieve(Integer(data))
		End If
	
  Case	"todoespecie"
		If	data = '1' Then
			This.Object.especie[row]		=	li_Null
			ii_especie = 0
		Else
			This.Object.especie[row]		=	li_Null
		End If

	Case "productor"
		If NOT iuo_Productores.Existe(Long(data),True,SqlCa) Then
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			Return -1
		Else
			ii_productor = Integer(data)
		End If
		
	Case "todoplanta"
		If data = '1' Then 
			This.Object.planta[row]		=	li_Null
			ii_planta = 0
		End If

	Case	"todoproductor"
		If	data = '1' Then 
			This.Object.productor[row]		=	li_Null
			ii_productor = 0
		End If
		
	Case "todolote"
		If	data = '1' Then 
			This.Object.lote[row]		=	li_Null
			ii_lote = 0
		End If
		
	Case "lote"
		If ii_especie = 0 Then
			ii_lote = Integer(data)
		ElseIf iuo_lotes.existe(ii_planta, ii_especie, integer(data), true, sqlca) Then
			ii_lote = Integer(data)
			This.Object.Productor[row] 	=	iuo_lotes.productor
			ii_productor 					=	iuo_lotes.productor
		Else
			This.Object.lote[row]		=	li_Null
			ii_lote = 0
			Return -1
		End If
		
		
End Choose
end event

event itemerror;Return -1
end event

event buttonclicked;String ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	case "b_lote"
		buscalote()
		This.object.todoproductor[row] = 1
END CHOOSE
end event

