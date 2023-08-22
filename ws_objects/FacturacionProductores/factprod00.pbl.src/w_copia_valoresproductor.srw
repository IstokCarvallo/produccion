$PBExportHeader$w_copia_valoresproductor.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_copia_valoresproductor from w_mant_detalle
end type
type st_3 from statictext within w_copia_valoresproductor
end type
type em_productor from editmask within w_copia_valoresproductor
end type
type cb_2 from uo_buscar within w_copia_valoresproductor
end type
type sle_nombre from singlelineedit within w_copia_valoresproductor
end type
type sle_zona from singlelineedit within w_copia_valoresproductor
end type
type st_1 from statictext within w_copia_valoresproductor
end type
end forward

global type w_copia_valoresproductor from w_mant_detalle
integer width = 3049
integer height = 1824
st_3 st_3
em_productor em_productor
cb_2 cb_2
sle_nombre sle_nombre
sle_zona sle_zona
st_1 st_1
end type
global w_copia_valoresproductor w_copia_valoresproductor

type variables
uo_Productores		iuo_Productores
end variables

forward prototypes
public function boolean compartedatos (long al_filaorigen, long al_filadestino, datawindow adw_origen, datawindow adw_destino)
public subroutine asignavalor (string as_columna, long al_fila, datawindow adw_origen, datawindow adw_destino)
end prototypes

public function boolean compartedatos (long al_filaorigen, long al_filadestino, datawindow adw_origen, datawindow adw_destino);Return True
end function

public subroutine asignavalor (string as_columna, long al_fila, datawindow adw_origen, datawindow adw_destino);//
end subroutine

on w_copia_valoresproductor.create
int iCurrent
call super::create
this.st_3=create st_3
this.em_productor=create em_productor
this.cb_2=create cb_2
this.sle_nombre=create sle_nombre
this.sle_zona=create sle_zona
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.em_productor
this.Control[iCurrent+3]=this.cb_2
this.Control[iCurrent+4]=this.sle_nombre
this.Control[iCurrent+5]=this.sle_zona
this.Control[iCurrent+6]=this.st_1
end on

on w_copia_valoresproductor.destroy
call super::destroy
destroy(this.st_3)
destroy(this.em_productor)
destroy(this.cb_2)
destroy(this.sle_nombre)
destroy(this.sle_zona)
destroy(this.st_1)
end on

event open;call super::open;x = 0
y = 0

iuo_Productores	=	Create uo_Productores	
end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	dw_1.Height + st_1.Height + 400
This.Width			=	dw_1.Width + 600

dw_1.x				=	78
dw_1.y				=	st_1.y + st_1.Height + 50	

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	dw_1.y

If pb_Acepta.Visible Then
	pb_Acepta.x			=	li_posic_x
	pb_Acepta.y			=	li_posic_y
	pb_Acepta.width	=	li_Ancho
	pb_Acepta.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

If pb_Cancela.Visible Then
	pb_Cancela.x		=	li_posic_x
	pb_Cancela.y		=	li_posic_y
	pb_Cancela.width	=	li_Ancho
	pb_Cancela.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

pb_salir.x			=	li_posic_x
pb_salir.y			=	li_posic_y
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_copia_valoresproductor
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_copia_valoresproductor
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_copia_valoresproductor
end type

type pb_primero from w_mant_detalle`pb_primero within w_copia_valoresproductor
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_copia_valoresproductor
integer x = 2683
integer y = 772
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_copia_valoresproductor
integer x = 2679
integer y = 512
end type

event pb_acepta::clicked;Long 		ll_Cliente, ll_Antiguo, ll_Nuevo
Integer	li_Control

ll_Cliente		=	Long(istr_Mant.Argumento[1])
ll_Antiguo	=	Long(istr_Mant.Argumento[2])
ll_Nuevo		=	Long(em_Productor.Text)

DECLARE	CopiaValores PROCEDURE FOR dbo.FGran_Copia_ValoresFacturaProductor
				@Cliente 	=	:ll_Cliente,   
				@Especie	=	-1,
				@Antiguo	=	:ll_Antiguo, 
				@Nuevo		=	:ll_Nuevo
			USING SQLCA ;
					
EXECUTE CopiaValores;	
			
If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA, "Copia de Valores Facturacion no se pudo cargar." )
Else
	FETCH CopiaValores into :li_control;
End If	
	
CLOSE  CopiaValores;

istr_mant.respuesta = 1
CloseWithReturn(Parent, istr_mant)
end event

type pb_salir from w_mant_detalle`pb_salir within w_copia_valoresproductor
integer x = 2693
integer y = 1044
end type

type dw_1 from w_mant_detalle`dw_1 within w_copia_valoresproductor
integer y = 500
integer width = 2487
integer height = 1156
boolean titlebar = true
string dataobject = "dw_mues_valofactprod"
boolean vscrollbar = true
boolean border = true
end type

type st_3 from statictext within w_copia_valoresproductor
integer x = 128
integer y = 188
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type em_productor from editmask within w_copia_valoresproductor
integer x = 507
integer y = 184
integer width = 219
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = ""
end type

event modified;If IsNull(This.Text) Or This.Text = '' Then Return

If Not iuo_Productores.Existe(Long(This.Text), True, Sqlca) Then 
	sle_nombre.text	= ""
	sle_zona.text		= ""
Else
	sle_nombre.text	= iuo_Productores.Nombre
	sle_zona.text		= iuo_Productores.NombreZona
	
	dw_1.Retrieve(Long(Istr_Mant.Argumento[1]), Long(This.Text))
End If
end event

type cb_2 from uo_buscar within w_copia_valoresproductor
integer x = 745
integer y = 184
integer width = 96
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

event clicked;str_Busqueda lstr_Busq
lstr_Busq.Argum[1]  = ''

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) > 3 Then
	If lstr_Busq.argum[1] = "" Then
		em_productor.SetFocus()
	Else
		em_productor.Text	= lstr_Busq.argum[1]
		sle_nombre.Text		= lstr_Busq.argum[2]
		sle_zona.Text			= lstr_Busq.argum[4]
		
		dw_1.Retrieve(Long(Istr_Mant.Argumento[1]), Long(em_productor.Text))
	End If
End If
end event

type sle_nombre from singlelineedit within w_copia_valoresproductor
integer x = 859
integer y = 184
integer width = 1627
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_zona from singlelineedit within w_copia_valoresproductor
integer x = 507
integer y = 300
integer width = 1979
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_copia_valoresproductor
integer x = 78
integer y = 140
integer width = 2487
integer height = 316
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
borderstyle borderstyle = StyleRaised!
boolean focusrectangle = false
end type

