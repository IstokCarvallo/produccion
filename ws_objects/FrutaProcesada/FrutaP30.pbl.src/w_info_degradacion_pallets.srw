$PBExportHeader$w_info_degradacion_pallets.srw
$PBExportComments$Informe Histórico de Control de Calidad.
forward
global type w_info_degradacion_pallets from w_para_informes
end type
type st_1 from statictext within w_info_degradacion_pallets
end type
type st_2 from statictext within w_info_degradacion_pallets
end type
type st_3 from statictext within w_info_degradacion_pallets
end type
type cb_pallet from commandbutton within w_info_degradacion_pallets
end type
type em_pallet from editmask within w_info_degradacion_pallets
end type
type st_4 from statictext within w_info_degradacion_pallets
end type
type em_fech_ini from editmask within w_info_degradacion_pallets
end type
type em_fech_fin from editmask within w_info_degradacion_pallets
end type
type st_5 from statictext within w_info_degradacion_pallets
end type
type st_6 from statictext within w_info_degradacion_pallets
end type
type cbx_1 from checkbox within w_info_degradacion_pallets
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_degradacion_pallets
end type
type uo_selplanta from uo_seleccion_plantas within w_info_degradacion_pallets
end type
end forward

global type w_info_degradacion_pallets from w_para_informes
integer width = 2674
integer height = 1420
st_1 st_1
st_2 st_2
st_3 st_3
cb_pallet cb_pallet
em_pallet em_pallet
st_4 st_4
em_fech_ini em_fech_ini
em_fech_fin em_fech_fin
st_5 st_5
st_6 st_6
cbx_1 cbx_1
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_degradacion_pallets w_info_degradacion_pallets

type variables
str_Mant	istr_Mant
end variables

forward prototypes
public function boolean noexistepallet (long al_numero)
public function boolean existe_lote (long al_numero)
end prototypes

public function boolean noexistepallet (long al_numero);String		ls_nomvar, ls_embala
Integer	 li_ContCalidad

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

public function boolean existe_lote (long al_numero);String	ls_nomvar, ls_embala
Integer	li_Cont

SELECT	count(*)
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

on w_info_degradacion_pallets.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.cb_pallet=create cb_pallet
this.em_pallet=create em_pallet
this.st_4=create st_4
this.em_fech_ini=create em_fech_ini
this.em_fech_fin=create em_fech_fin
this.st_5=create st_5
this.st_6=create st_6
this.cbx_1=create cbx_1
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.cb_pallet
this.Control[iCurrent+5]=this.em_pallet
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.em_fech_ini
this.Control[iCurrent+8]=this.em_fech_fin
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.cbx_1
this.Control[iCurrent+12]=this.uo_selcliente
this.Control[iCurrent+13]=this.uo_selplanta
end on

on w_info_degradacion_pallets.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.cb_pallet)
destroy(this.em_pallet)
destroy(this.st_4)
destroy(this.em_fech_ini)
destroy(this.em_fech_fin)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.cbx_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	
	uo_SelPlanta.Filtra(1)

	uo_SelCliente.Inicia(gi_codexport)

	em_fech_ini.Text			=	String(Today())
	em_fech_fin.Text			=	String(Today())
	istr_mant.argumento[3]	=	String(Today())
	istr_mant.argumento[4]	=	String(Today())
	istr_mant.argumento[5]	=	'-1'
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_degradacion_pallets
end type

type st_computador from w_para_informes`st_computador within w_info_degradacion_pallets
end type

type st_usuario from w_para_informes`st_usuario within w_info_degradacion_pallets
end type

type st_temporada from w_para_informes`st_temporada within w_info_degradacion_pallets
end type

type p_logo from w_para_informes`p_logo within w_info_degradacion_pallets
end type

type st_titulo from w_para_informes`st_titulo within w_info_degradacion_pallets
integer width = 1883
string text = "Bitácora Degradación de Pallets"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_degradacion_pallets
integer x = 2258
integer y = 636
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_NroPallet

istr_info.titulo	=	"BITACORA DEGRADACION DE CALIDAD PALLETS"
istr_info.copias	=	1

ll_NroPallet		=	Long(istr_mant.argumento[5])

IF ll_NroPallet = 0 THEN
	ll_NroPallet = -1
END IF	

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_degradacion_pallets"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, &
									  ll_NroPallet,date(istr_mant.argumento[3]),date(istr_mant.argumento[4]))

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_fechaini.text = '" + em_fech_ini.text + "'")	
   	vinf.dw_1.Modify("t_fechafin.text = '" + em_fech_fin.text + "'")		
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_degradacion_pallets
integer x = 2263
integer y = 936
integer taborder = 90
end type

type st_1 from statictext within w_info_degradacion_pallets
integer x = 247
integer y = 412
integer width = 1883
integer height = 768
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

type st_2 from statictext within w_info_degradacion_pallets
integer x = 338
integer y = 472
integer width = 274
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_degradacion_pallets
integer x = 338
integer y = 976
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

type cb_pallet from commandbutton within w_info_degradacion_pallets
integer x = 1298
integer y = 968
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
	
	em_pallet.Text				=	lstr_busq.Argum[2]
	istr_mant.argumento[5] 	=	lstr_busq.Argum[2]
ELSE
	em_pallet.SetFocus()
END IF
end event

type em_pallet from editmask within w_info_degradacion_pallets
integer x = 882
integer y = 964
integer width = 402
integer height = 92
integer taborder = 60
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

event modified;If IsNull(This.Text) or This.Text = '' Then Return

IF NoExistePallet(Long(This.Text)) AND Long(This.Text) <> 0 THEN
	This.Text	=	""
	This.SetFocus()
ELSE
	istr_mant.argumento[5] = This.Text
END IF
end event

type st_4 from statictext within w_info_degradacion_pallets
integer x = 338
integer y = 804
integer width = 325
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
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_degradacion_pallets
integer x = 882
integer y = 796
integer width = 489
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type em_fech_fin from editmask within w_info_degradacion_pallets
integer x = 1605
integer y = 796
integer width = 489
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_5 from statictext within w_info_degradacion_pallets
integer x = 338
integer y = 672
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_degradacion_pallets
integer x = 1399
integer y = 804
integer width = 197
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
string text = "Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_info_degradacion_pallets
integer x = 1458
integer y = 972
integer width = 402
integer height = 72
integer taborder = 70
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

event clicked;IF This.Checked THEN
	istr_mant.argumento[5]	=	'-1'
	em_pallet.Enabled			=	False
	cb_pallet.Enabled			=	False
	em_pallet.Text				=	''
ELSE
	em_pallet.Enabled			=	True
	cb_pallet.Enabled			=	True
	em_pallet.SetFocus()
END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_degradacion_pallets
event destroy ( )
integer x = 869
integer y = 460
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_degradacion_pallets
event destroy ( )
integer x = 869
integer y = 580
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

