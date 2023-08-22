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
integer width = 2030
integer height = 920
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
str_mant	istr_mant
end variables

forward prototypes
public function boolean existe (long al_pallet)
end prototypes

public function boolean existe (long al_pallet);Integer	li_cliente, li_existe
Long		ll_planta
Boolean	lb_retorno = True

li_cliente	=	Integer(istr_mant.Argumento[1])
ll_planta	=	Long(istr_mant.Argumento[2])

SELECT Count(*)
  INTO :li_existe
  FROM dbo.spro_palletencab
 WHERE clie_codigo = :li_cliente
   AND plde_codigo = :ll_planta
	AND paen_numero = :al_pallet
USING sqlca;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = False
ELSEIF li_existe > 0 THEN
	MessageBox("Error", "El Pallet ingresado ya existe. ~r~nPor favor, Ingrese o seleccione otro")
	lb_retorno = False
END IF

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

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.InsertRow(0)

dw_1.Object.Actual[1]	=	Long(istr_mant.Argumento[3])
istr_mant.Argumento[4]	=	''
end event

type pb_salir from picturebutton within w_cambio_folio_pallets
event mousemove pbm_mousemove
string tag = "Salir"
integer x = 1678
integer y = 532
integer width = 302
integer height = 244
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "1"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
string powertiptext = "Salir"
end type

event clicked;istr_mant.respuesta = 0

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from picturebutton within w_cambio_folio_pallets
event mousemove pbm_mousemove
string tag = "Aceptar Acción"
integer x = 1678
integer y = 28
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "1"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
string powertiptext = "Aceptar Acción"
end type

event clicked;Integer	li_null
istr_mant.respuesta = 1
dw_1.AcceptText()

IF NOT Existe(dw_1.Object.nuevo[1]) THEN
	dw_1.Object.Nuevo[1]		=	li_null
	istr_mant.Argumento[4]	=	''
	Return 1
	
ELSE
	istr_mant.Argumento[4]	=	String(dw_1.Object.nuevo[1])
	CloseWithReturn(Parent, istr_mant)
	
END IF
		
end event

type pb_cancela from picturebutton within w_cambio_folio_pallets
event mousemove pbm_mousemove
string tag = "Rechazar Acción"
integer x = 1678
integer y = 276
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "1"
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
string powertiptext = "Rechazar Acción"
end type

event clicked;istr_mant.respuesta = 2

CloseWithReturn(Parent, istr_mant)
end event

type dw_1 from datawindow within w_cambio_folio_pallets
integer x = 69
integer y = 212
integer width = 1509
integer height = 360
integer taborder = 10
string title = "Cambio de Folio"
string dataobject = "dw_cambio_folio_pallet"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_columna
Integer	li_null

ls_columna	=	dwo.Name
SetNull(li_null)

CHOOSE CASE ls_columna
	CASE "nuevo"
		IF NOT Existe(Long(data)) THEN
			This.Object.Nuevo[1]		=	li_null
			istr_mant.Argumento[4]	=	''
			Return 1
			
		ELSE
			istr_mant.Argumento[4]	=	Data
			
		END IF
		
END CHOOSE
end event

event itemerror;Return 1
end event

