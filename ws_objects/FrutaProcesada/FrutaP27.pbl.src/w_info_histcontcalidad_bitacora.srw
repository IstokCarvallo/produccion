$PBExportHeader$w_info_histcontcalidad_bitacora.srw
$PBExportComments$Informe Histórico de Control de Calidad.
forward
global type w_info_histcontcalidad_bitacora from w_para_informes
end type
type st_1 from statictext within w_info_histcontcalidad_bitacora
end type
type st_2 from statictext within w_info_histcontcalidad_bitacora
end type
type st_3 from statictext within w_info_histcontcalidad_bitacora
end type
type cb_pallet from commandbutton within w_info_histcontcalidad_bitacora
end type
type em_pallet from editmask within w_info_histcontcalidad_bitacora
end type
type st_6 from statictext within w_info_histcontcalidad_bitacora
end type
type em_lote from editmask within w_info_histcontcalidad_bitacora
end type
type st_4 from statictext within w_info_histcontcalidad_bitacora
end type
type st_5 from statictext within w_info_histcontcalidad_bitacora
end type
type em_desde from editmask within w_info_histcontcalidad_bitacora
end type
type em_hasta from editmask within w_info_histcontcalidad_bitacora
end type
type cbx_fecha from checkbox within w_info_histcontcalidad_bitacora
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_histcontcalidad_bitacora
end type
type cbx_pallet from checkbox within w_info_histcontcalidad_bitacora
end type
type cbx_lote from checkbox within w_info_histcontcalidad_bitacora
end type
end forward

global type w_info_histcontcalidad_bitacora from w_para_informes
integer width = 2702
integer height = 1320
st_1 st_1
st_2 st_2
st_3 st_3
cb_pallet cb_pallet
em_pallet em_pallet
st_6 st_6
em_lote em_lote
st_4 st_4
st_5 st_5
em_desde em_desde
em_hasta em_hasta
cbx_fecha cbx_fecha
uo_selcliente uo_selcliente
cbx_pallet cbx_pallet
cbx_lote cbx_lote
end type
global w_info_histcontcalidad_bitacora w_info_histcontcalidad_bitacora

forward prototypes
public function boolean noexistepallet (long al_numero)
public function boolean existe_lote (long al_numero)
end prototypes

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_ContCalidad


SELECT	paen_concal
	INTO	:li_ContCalidad
	FROM	dbo.palletencab
	WHERE clie_codigo	= :uo_SelCliente.Codigo
	AND	paen_numero	= :al_numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado.~r~r" +&
					"Ingrese o seleccione otro Número.", Exclamation!, OK!)
	RETURN True
ELSE
		
	RETURN False
END IF
end function

public function boolean existe_lote (long al_numero);String		ls_nomvar, ls_embala
Integer	 li_Cont


SELECT	Count(*)
	INTO	:li_Cont
	FROM	dbo.palletfruta
	WHERE clie_codigo	= :uo_SelCliente.Codigo
	AND	pafr_nrlote	= :al_numero;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla palletfruta")
	RETURN True
ELSEIF li_Cont = 0 THEN
	MessageBox("Atención", "Número de Lote no ha sido creado.~r~r" +&
					"Ingrese o seleccione otro Número.", Exclamation!, OK!)
	RETURN True
ELSE
		
	RETURN False
END IF
end function

on w_info_histcontcalidad_bitacora.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.cb_pallet=create cb_pallet
this.em_pallet=create em_pallet
this.st_6=create st_6
this.em_lote=create em_lote
this.st_4=create st_4
this.st_5=create st_5
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.cbx_fecha=create cbx_fecha
this.uo_selcliente=create uo_selcliente
this.cbx_pallet=create cbx_pallet
this.cbx_lote=create cbx_lote
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.cb_pallet
this.Control[iCurrent+5]=this.em_pallet
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.em_lote
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.em_desde
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.cbx_fecha
this.Control[iCurrent+13]=this.uo_selcliente
this.Control[iCurrent+14]=this.cbx_pallet
this.Control[iCurrent+15]=this.cbx_lote
end on

on w_info_histcontcalidad_bitacora.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.cb_pallet)
destroy(this.em_pallet)
destroy(this.st_6)
destroy(this.em_lote)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.cbx_fecha)
destroy(this.uo_selcliente)
destroy(this.cbx_pallet)
destroy(this.cbx_lote)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_codexport)
	
	em_desde.Text = String(RelativeDate(Today(), -365))
	em_hasta.Text = String(today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_histcontcalidad_bitacora
end type

type st_computador from w_para_informes`st_computador within w_info_histcontcalidad_bitacora
end type

type st_usuario from w_para_informes`st_usuario within w_info_histcontcalidad_bitacora
end type

type st_temporada from w_para_informes`st_temporada within w_info_histcontcalidad_bitacora
end type

type p_logo from w_para_informes`p_logo within w_info_histcontcalidad_bitacora
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_histcontcalidad_bitacora
integer width = 1883
string text = "Informe Histórico Control de Calidad"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_histcontcalidad_bitacora
integer x = 2267
integer y = 484
integer taborder = 30
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_NroPallet, ll_lote
Date		ld_fdesde, ld_fhasta

istr_info.titulo	=	"BITACORA COTROL DE CALIDAD PALLETS"
istr_info.copias	=	1

ld_fdesde = Date(em_desde.Text)
ld_fhasta = Date(em_hasta.Text)

If cbx_Pallet.Checked Then 
	ll_NroPallet = -1
Else
	ll_NroPallet	 =	Long(em_pallet.Text)	
End If

If cbx_Lote.Checked Then
	ll_lote = -1
Else
	ll_lote	 =	Long(em_lote.Text)
End If

OpenWithParm(vinf, istr_info)

If cbx_fecha.Checked Then
	vinf.dw_1.DataObject = "dw_info_histocontcalidad_bitacora_fecha"
Else
	vinf.dw_1.DataObject = "dw_info_histocontcalidad_bitacora"
End If	

vinf.dw_1.SetTransObject(sqlca)

ll_Fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, ll_NroPallet,ll_lote,ld_fdesde,ld_fhasta)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_histcontcalidad_bitacora
integer x = 2267
integer y = 784
integer taborder = 40
end type

type st_1 from statictext within w_info_histcontcalidad_bitacora
integer x = 251
integer y = 440
integer width = 1883
integer height = 696
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_histcontcalidad_bitacora
integer x = 375
integer y = 508
integer width = 274
integer height = 52
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_histcontcalidad_bitacora
integer x = 370
integer y = 648
integer width = 430
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número Pallet"
boolean focusrectangle = false
end type

type cb_pallet from commandbutton within w_info_histcontcalidad_bitacora
integer x = 1294
integer y = 640
integer width = 96
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
lstr_busq.argum[2]	=	""
lstr_busq.argum[12]	=	String(9999)

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	NoExistePallet(Long(lstr_busq.Argum[2]))
	
	em_pallet.Text	=	lstr_busq.Argum[2]
ELSE
	em_pallet.SetFocus()
END IF
end event

type em_pallet from editmask within w_info_histcontcalidad_bitacora
integer x = 864
integer y = 636
integer width = 402
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;em_lote.Text = ''
If This.Text = '' Or IsNull(This.Text) Then Return

IF NoExistePallet(Long(This.Text)) AND Long(This.Text) <> 0 THEN
	This.Text	=	""
	This.SetFocus()
END IF
end event

type st_6 from statictext within w_info_histcontcalidad_bitacora
integer x = 370
integer y = 784
integer width = 430
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lote"
boolean focusrectangle = false
end type

type em_lote from editmask within w_info_histcontcalidad_bitacora
integer x = 864
integer y = 772
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;em_pallet.Text = ''
If This.Text = '' Or IsNull(This.Text) Then Return

IF Existe_lote(Long(This.Text)) AND Long(This.Text) <> 0 THEN
	This.Text	=	""
	This.SetFocus()
END IF

end event

type st_4 from statictext within w_info_histcontcalidad_bitacora
integer x = 370
integer y = 948
integer width = 430
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Desde"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_histcontcalidad_bitacora
integer x = 1280
integer y = 948
integer width = 430
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Hasta"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_histcontcalidad_bitacora
integer x = 864
integer y = 924
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;em_pallet.Text = ''

IF existe_lote(Long(This.Text)) AND Long(This.Text) <> 0 THEN
	This.Text	=	""
	This.SetFocus()
END IF

end event

type em_hasta from editmask within w_info_histcontcalidad_bitacora
integer x = 1687
integer y = 924
integer width = 402
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;em_pallet.Text = ''

IF existe_lote(Long(This.Text)) AND Long(This.Text) <> 0 THEN
	This.Text	=	""
	This.SetFocus()
END IF

end event

type cbx_fecha from checkbox within w_info_histcontcalidad_bitacora
integer x = 370
integer y = 1036
integer width = 741
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Ordenado Por Fecha"
boolean lefttext = true
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_histcontcalidad_bitacora
integer x = 864
integer y = 488
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_pallet from checkbox within w_info_histcontcalidad_bitacora
integer x = 1467
integer y = 640
integer width = 329
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_pallet.Enabled	=	False
	cb_pallet.Enabled	=	False
	em_pallet.Text		=	''
Else
	em_pallet.Enabled	=	True
	cb_pallet.Enabled	=	True
End If
end event

type cbx_lote from checkbox within w_info_histcontcalidad_bitacora
integer x = 1467
integer y = 776
integer width = 329
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_lote.Enabled	=	False
	em_lote.Text		=	''
Else
	em_lote.Enabled	=	True
End If
end event

