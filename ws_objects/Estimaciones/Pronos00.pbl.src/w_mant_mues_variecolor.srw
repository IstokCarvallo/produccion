$PBExportHeader$w_mant_mues_variecolor.srw
$PBExportComments$Ventana de Asociación de Categorías por Especie
forward
global type w_mant_mues_variecolor from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_variecolor
end type
type st_2 from statictext within w_mant_mues_variecolor
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_variecolor
end type
type ddlb_colores from dropdownlistbox within w_mant_mues_variecolor
end type
end forward

global type w_mant_mues_variecolor from w_mant_directo
integer width = 2075
string title = "Colores de Variedad"
st_1 st_1
st_2 st_2
uo_selespecie uo_selespecie
ddlb_colores ddlb_colores
end type
global w_mant_mues_variecolor w_mant_mues_variecolor

type variables
DatawindowChild idwc_Variedad

Integer ii_Color
String is_Sentencias[], is_sintaxis

uo_Variedades	iuo_Variedades
end variables

forward prototypes
public function boolean duplicado (integer ai_categoria)
end prototypes

public function boolean duplicado (integer ai_categoria);Long		ll_fila
String	ls_codigo

ls_codigo	= String(dw_1.object.vari_codigo [il_fila])
	
ll_fila	= dw_1.Find("vari_codigo = " + string(ai_categoria), 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Variedad ya Fue Ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
		
end function

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close (This)
Else
	uo_SelEspecie.Seleccion (False, False)
	
	iuo_Variedades = 	Create uo_Variedades
	
	dw_1.GetChild('vari_codigo', idwc_Variedad)
	idwc_Variedad.SetTransObject(Sqlca)
	idwc_Variedad.Retrieve(-1)
	
	buscar		=	"Variedad:Nvari_codigo"
	ordenar		=	"Variedad:vari_codigo"
	is_ultimacol	=	'vari_codigo'
End If
end event

on w_mant_mues_variecolor.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selespecie=create uo_selespecie
this.ddlb_colores=create ddlb_colores
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selespecie
this.Control[iCurrent+4]=this.ddlb_colores
end on

on w_mant_mues_variecolor.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selespecie)
destroy(this.ddlb_colores)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer li_Null
Long ll_fila, respuesta

SetNull(li_Null)
	
DO
	ll_Fila	= dw_1.Retrieve(uo_SelEspecie.Codigo, ii_Color)

	IF ll_Fila = -1 THEN
		Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_Fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE Respuesta = 1

IF Respuesta = 2 THEN Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)
Integer	li_Null, li_respuesta
Long		fila
String	ls_argumento, ls_argum1

SetNull(li_Null)
str_info	lstr_info

lstr_info.titulo	= "Colores por Variedad"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_varicolor"
vinf.dw_1.SetTransObject(sqlca)

Fila	= vinf.dw_1.Retrieve(uo_SelEspecie.Codigo, ii_Color)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;Long	ll_fila = 1

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSEIF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		IF NOT Isnull(dw_1.Object.vari_codigo[ll_fila]) THEN
			dw_1.Object.espe_codigo[ll_fila] = uo_SelEspecie.Codigo
			dw_1.Object.vaco_codigo[ll_fila] = ii_Color
			ll_fila ++
		ELSE
			dw_1.DeleteRow(ll_fila)
		END IF
	ELSE
		ll_fila ++		
	END IF
LOOP
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_variecolor
integer x = 96
integer width = 1394
integer height = 340
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_variecolor
integer x = 1714
integer y = 424
integer taborder = 30
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_variecolor
integer x = 1673
integer y = 72
integer taborder = 20
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_variecolor
integer x = 1714
integer y = 784
integer taborder = 60
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_variecolor
integer x = 1714
integer y = 604
integer taborder = 50
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_variecolor
integer x = 1714
integer y = 1528
integer taborder = 90
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_variecolor
integer x = 1714
integer y = 1144
integer taborder = 80
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_variecolor
integer x = 1714
integer y = 964
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_variecolor
integer x = 91
integer y = 488
integer width = 1408
integer height = 1364
integer taborder = 40
string dataobject = "dw_mues_varicolor"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna 
Integer	li_Null

SetNull(li_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "vari_codigo"
	  	If Duplicado(Integer(Data)) Or Not iuo_Variedades.Existe(uo_SelEspecie.Codigo, Integer(Data), True, SqlCa) Then
			 This.SetItem(il_fila, ls_Columna, li_Null) 
		  	Return 1		  
		End If
End Choose
		
		
end event

event dw_1::sqlpreview;IF request = PreviewFunctionUpdate!	THEN
	is_sintaxis	=	sqlsyntax
END IF

CALL Super::SqlPreview
end event

event dw_1::on_delete;call super::on_delete;Integer	li_sentencias

li_sentencias	=	UpperBound(is_sentencias)

li_sentencias ++

is_sentencias[li_sentencias] = is_sintaxis
end event

event dw_1::on_insert;call super::on_insert;Integer	li_sentencias

li_sentencias	=	UpperBound(is_sentencias)

li_sentencias ++

is_sentencias[li_sentencias] = is_sintaxis
end event

event dw_1::on_update;call super::on_update;Integer	li_sentencias

li_sentencias	=	UpperBound(is_sentencias)

li_sentencias ++

is_sentencias[li_sentencias] = is_sintaxis
end event

type st_1 from statictext within w_mant_mues_variecolor
integer x = 183
integer y = 144
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_variecolor
integer x = 183
integer y = 256
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Color"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_variecolor
event destroy ( )
integer x = 507
integer y = 132
integer height = 88
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		dw_1.GetChild('vari_codigo', idwc_Variedad)
		idwc_Variedad.SetTransObject(Sqlca)
		idwc_Variedad.Retrieve(This.Codigo)
		
End Choose
end event

type ddlb_colores from dropdownlistbox within w_mant_mues_variecolor
integer x = 521
integer y = 252
integer width = 878
integer height = 632
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Blancas","Verdes","Rojas","Negras","Rojas C/Semilla","Negras C/Semilla","Tricolor"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_Color = Index
end event

