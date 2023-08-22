$PBExportHeader$w_info_lotes_entradasalida_comercial.srw
$PBExportComments$Informe Cartola de Movimientos Fruta Granel
forward
global type w_info_lotes_entradasalida_comercial from w_para_informes
end type
type dw_1 from datawindow within w_info_lotes_entradasalida_comercial
end type
end forward

global type w_info_lotes_entradasalida_comercial from w_para_informes
string tag = "Informe Movimientos Lotes Fruta Comercial"
integer x = 14
integer y = 32
integer width = 2560
integer height = 1424
string title = "Informe Movimientos Lotes Fruta Comercial"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
end type
global w_info_lotes_entradasalida_comercial w_info_lotes_entradasalida_comercial

type variables
uo_plantadesp					iuo_plantadesp
uo_especie						iuo_especie
uo_variedades					iuo_variedades
uo_productores				iuo_productores
uo_spro_lotesfrutacomenc	iuo_lotes

DataWindowChild	idwc_planta, idwc_especie, idwc_productor, idwc_exporta

String		is_NomPlanta
Integer	ii_Planta, ii_Especie, ii_productor, ii_lote, ii_cliente

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

OpenWithParm(w_busc_lotesfrutacomenc, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If lstr_busq.argum[1] = "" Then 
	dw_1.SetColumn("lote")
	dw_1.SetFocus()
Else 

	dw_1.Object.planta[1]	=	Integer(lstr_busq.argum[1])
	dw_1.Object.especie[1]	=	Integer(lstr_busq.argum[2])
	dw_1.Object.lote[1]		=	Integer(lstr_busq.argum[3])
	
	Istr_Mant.Argumento[1]	=	lstr_busq.argum[1]
	Istr_Mant.Argumento[2]	=	lstr_busq.argum[2]
	Istr_Mant.Argumento[3]	=	lstr_busq.argum[3]

	Istr_Mant.Argumento[4]			=	String(dw_1.Object.planta[1],"0000") + &
												String(dw_1.Object.especie[1],"00") + &
												String(dw_1.Object.lote[1],"0000")

	dw_1.SetColumn("lote")
	dw_1.SetFocus()
End If

Return
end subroutine

on w_info_lotes_entradasalida_comercial.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_lotes_entradasalida_comercial.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_productores		=	Create uo_productores
iuo_lotes					= 	Create uo_spro_lotesfrutacomenc

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

//Especie
dw_1.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF


//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(-1) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechadesde", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechahasta", Today())
dw_1.Setitem(1,"cliente",gi_codexport)
dw_1.Setitem(1,"especie",gstr_ParamPlanta.CodigoEspecie)
dw_1.SetItem(1, "planta",gstr_ParamPlanta.CodigoPlanta )

ii_planta 		= 	gstr_ParamPlanta.CodigoPlanta
ii_cliente 		= 	gi_codexport
ii_especie 	=	gstr_ParamPlanta.CodigoEspecie
end event

type pb_excel from w_para_informes`pb_excel within w_info_lotes_entradasalida_comercial
end type

type st_computador from w_para_informes`st_computador within w_info_lotes_entradasalida_comercial
integer x = 1010
integer width = 1431
end type

type st_usuario from w_para_informes`st_usuario within w_info_lotes_entradasalida_comercial
integer x = 1010
integer width = 1431
end type

type st_temporada from w_para_informes`st_temporada within w_info_lotes_entradasalida_comercial
integer x = 1010
integer width = 1431
end type

type p_logo from w_para_informes`p_logo within w_info_lotes_entradasalida_comercial
end type

type st_titulo from w_para_informes`st_titulo within w_info_lotes_entradasalida_comercial
integer width = 1696
string text = "Informe Movimientos Lotes Fruta Comercial"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_lotes_entradasalida_comercial
integer x = 2185
integer y = 532
integer height = 240
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie,  li_Variedad, li_Productor,li_lote
Long		ll_Fila
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba

istr_info.titulo	= "MOVIMIENTOS DE LOTES"
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_informe_movimientos_lotes01"
dw_1.accepttext()

// Acepta Planta //
IF dw_1.Object.todoplanta[1] = 1 THEN
	li_Planta = -1
ELSE
	li_Planta = dw_1.Object.planta[1]
	IF IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Especie //
IF dw_1.Object.todoespecie[1] = 1 THEN
	li_Especie = -1
ELSE
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todoproductor[1] = 1 THEN
	li_Productor = -1
ELSE
	li_Productor = dw_1.Object.productor[1]
	If IsNull(li_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Lote
IF dw_1.Object.todoLote[1] = 1 THEN
	li_Lote = -1
ELSE
	li_lote = dw_1.Object.lote[1]
	If IsNull(li_lote) Then
		MessageBox( "Lote Erróneo", "Falta Ingresar un número de lote correcto.", StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

ld_fechadesde = dw_1.Object.fechadesde[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	RETURN 1				 
END If

ld_fechahasta = 	dw_1.Object.fechahasta[1]
If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	RETURN 1				 
END If

IF ld_fechadesde > ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", StopSign!, Ok!)
	RETURN 1
END IF	

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, li_Especie, li_Productor, ld_fechadesde, ld_fechahasta, ii_cliente, li_lote)

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

type pb_salir from w_para_informes`pb_salir within w_info_lotes_entradasalida_comercial
integer x = 2185
integer y = 804
integer height = 228
integer taborder = 380
end type

type dw_1 from datawindow within w_info_lotes_entradasalida_comercial
integer x = 247
integer y = 440
integer width = 1696
integer height = 688
integer taborder = 380
boolean bringtotop = true
string title = "none"
string dataobject = "dw_seleccion_lotes_entradasalida"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)
li_cliente = dw_1.Object.cliente[1]
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "planta"
		If Not iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			Return 1
		Else 
			dw_1.Object.todoplanta.protect = 0
			ii_planta = Integer(data)
		End If
		
	Case "cliente"
		ii_cliente = Integer(data)
		
	Case "especie"
		If Not iuo_Especie.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			Return 1
		Else
			ii_especie = Integer(data)
		End If
	
  Case	"todoespecie"
		If	data = '1' Then
			This.Object.especie[row]		=	li_Null
			ii_especie = -1
		Else
			This.Object.especie[row]		=	li_Null
		End If

	Case "productor"
		If Not iuo_Productores.Existe(Long(data),True,SqlCa) Then
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			Return 1
		Else
			ii_productor = Integer(data)
		End If
		
	Case "todoplanta"
		If data = '1' Then 
			This.Object.planta[row]		=	li_Null
			ii_planta = -1
		End If

	Case	"todoproductor"
		If	data = '1' Then 
			This.Object.productor[row]		=	li_Null
			ii_productor = -1
		End If
		
	Case "todolote"
		If	data = '1' Then 
			This.Object.lote[row]		=	li_Null
			ii_lote = -1
		End If
		
	Case "lote"
		If ii_especie = -1 Then
			ii_lote = Integer(data)
		ElseIf iuo_lotes.existeLote(ii_planta, ii_especie, Long(data), sqlca, True) Then
			ii_lote = Integer(data)
			This.Object.Productor[Row]	=	iuo_lotes.Productor
			This.Object.Planta[Row]			=	iuo_Lotes.Planta
			This.Object.Cliente[Row]		=	iuo_Lotes.Exportador
			ii_productor 						=	iuo_lotes.Productor
			ii_planta								=	iuo_Lotes.Planta
		Else
			This.Object.lote[row]		=	li_Null
			ii_lote = -1
			Return 1
		End If
			
End Choose
end event

event itemerror;Return 1
end event

event buttonclicked;String ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	case "b_lote"
		buscalote()
		This.object.todoproductor[row] = 1
END CHOOSE
end event

