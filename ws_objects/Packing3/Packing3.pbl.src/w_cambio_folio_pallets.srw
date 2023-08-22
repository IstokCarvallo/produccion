$PBExportHeader$w_cambio_folio_pallets.srw
forward
global type w_cambio_folio_pallets from window
end type
type pb_salir from picturebutton within w_cambio_folio_pallets
end type
type pb_acepta from picturebutton within w_cambio_folio_pallets
end type
type pb_cancela from picturebutton within w_cambio_folio_pallets
end type
type dw_1 from datawindow within w_cambio_folio_pallets
end type
end forward

global type w_cambio_folio_pallets from window
integer width = 1806
integer height = 1008
boolean titlebar = true
string title = "CAMBIO DE FOLIO PALLETS"
windowtype windowtype = response!
long backcolor = 16777215
string icon = "AppIcon!"
boolean center = true
pb_salir pb_salir
pb_acepta pb_acepta
pb_cancela pb_cancela
dw_1 dw_1
end type
global w_cambio_folio_pallets w_cambio_folio_pallets

type variables
str_mant						istr_mant
uo_ControlVentanas		iuo_Ventana
end variables
forward prototypes
public function boolean existe (long al_pallet)
end prototypes

public function boolean existe (long al_pallet);Integer	li_cliente, li_existe
Long		ll_planta
Boolean	lb_retorno

li_cliente	=	Integer(istr_mant.Argumento[1])
ll_planta	=	Long(istr_mant.Argumento[2])

SELECT Count(*)
  INTO :li_existe
  FROM dbo.spro_palletencab
 WHERE clie_codigo = :li_cliente
   AND plde_codigo = :ll_planta
	AND paen_numero = :al_pallet
USING sqlca;

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = False
ElseIf li_existe > 0 Then
	MessageBox("Error", "El Pallet ingresado ya existe. ~r~nPor favor, Ingrese o seleccione otro")
	lb_retorno = False
Else
	lb_retorno = True
End If

Return lb_retorno
end function

on w_cambio_folio_pallets.create
this.pb_salir=create pb_salir
this.pb_acepta=create pb_acepta
this.pb_cancela=create pb_cancela
this.dw_1=create dw_1
this.Control[]={this.pb_salir,&
this.pb_acepta,&
this.pb_cancela,&
this.dw_1}
end on

on w_cambio_folio_pallets.destroy
destroy(this.pb_salir)
destroy(this.pb_acepta)
destroy(this.pb_cancela)
destroy(this.dw_1)
end on

event open;Long	ll_Correlativo
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

istr_mant = Message.PowerObjectParm
iuo_Ventana	=	Create uo_ControlVentanas

dw_1.InsertRow(0)

dw_1.Object.Actual[1]	=	Long(istr_mant.Argumento[3])
istr_mant.Argumento[5]	=	''

If istr_mant.Argumento[4] = '1' Then
	dw_1.Object.Nuevo.Protect = 1
	dw_1.Object.Nuevo.Background.Color	= 553648127
	dw_1.Object.Nuevo.Color				=	RGB(255, 255, 255)
	
	ll_Correlativo =	iuo_Ventana.Correlativo(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[2]), Long(istr_mant.Argumento[4]))
	If ll_Correlativo <> -1 Then
		dw_1.Object.Nuevo[1]	= ll_Correlativo
		istr_Mant.Argumento[5]	=	String(ll_Correlativo)
		iuo_Ventana.of_Disponible(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[2]), Long(istr_mant.Argumento[4]), SQLCA)
	Else
		Message.DoubleParm	=	-1
		Return
	End If
Else
	
	
End If
end event

type pb_salir from picturebutton within w_cambio_folio_pallets
event mousemove pbm_mousemove
string tag = "Salir"
integer x = 1435
integer y = 608
integer width = 302
integer height = 244
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
string powertiptext = "Salir"
long backcolor = 553648127
end type

event clicked;istr_mant.respuesta = 0

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from picturebutton within w_cambio_folio_pallets
event mousemove pbm_mousemove
string tag = "Aceptar Acción"
integer x = 1435
integer y = 64
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
string powertiptext = "Aceptar Acción"
long backcolor = 553648127
end type

event clicked;istr_mant.respuesta = 1

CloseWithReturn(Parent, istr_mant)
end event

type pb_cancela from picturebutton within w_cambio_folio_pallets
event mousemove pbm_mousemove
string tag = "Rechazar Acción"
integer x = 1435
integer y = 336
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
string powertiptext = "Rechazar Acción"
long backcolor = 553648127
end type

event clicked;istr_mant.respuesta = 2

CloseWithReturn(Parent, istr_mant)
end event

type dw_1 from datawindow within w_cambio_folio_pallets
integer x = 91
integer y = 228
integer width = 1248
integer height = 376
integer taborder = 10
string title = "Cambio de Folio"
string dataobject = "dw_cambio_folio_pallet"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_columna
Integer	ll_Null

ls_columna	=	dwo.Name
SetNull(ll_Null)

Choose Case ls_columna
	Case "nuevo"
		If istr_Mant.Argumento[4] = '2'Then
			If Existe(Long(data))  Or Not iuo_Ventana.ValidaRango(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[2]), Long(Data), 2, True, SQLCA) Then
				This.SetItem(1, "nuevo", ll_Null)
				Return 1
			Else
				iuo_Ventana.of_Disponible(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[2]), 2, SQLCA)
			End If
		End If
		
		istr_mant.Argumento[5]	=	Data
		
End Choose
end event

event itemerror;Return 1
end event

