$PBExportHeader$w_info_procesoshistoricosunpallet.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_procesoshistoricosunpallet from w_para_informes
end type
type st_4 from statictext within w_info_procesoshistoricosunpallet
end type
type st_5 from statictext within w_info_procesoshistoricosunpallet
end type
type st_2 from statictext within w_info_procesoshistoricosunpallet
end type
type em_numero from editmask within w_info_procesoshistoricosunpallet
end type
type dw_2 from datawindow within w_info_procesoshistoricosunpallet
end type
type st_6 from statictext within w_info_procesoshistoricosunpallet
end type
type cb_buscarepa from commandbutton within w_info_procesoshistoricosunpallet
end type
type cbx_varirotula from checkbox within w_info_procesoshistoricosunpallet
end type
end forward

global type w_info_procesoshistoricosunpallet from w_para_informes
integer x = 14
integer y = 32
integer width = 2665
integer height = 1228
string title = "Procesos Históricos Pallet"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_5 st_5
st_2 st_2
em_numero em_numero
dw_2 dw_2
st_6 st_6
cb_buscarepa cb_buscarepa
cbx_varirotula cbx_varirotula
end type
global w_info_procesoshistoricosunpallet w_info_procesoshistoricosunpallet

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente
end variables

on w_info_procesoshistoricosunpallet.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_numero
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_buscarepa
this.Control[iCurrent+8]=this.cbx_varirotula
end on

on w_info_procesoshistoricosunpallet.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.cbx_varirotula)
end on

event open;call super::open;String	ls_Columna[]

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

IF ExisteCliente(gi_CodExport, ls_Columna[]) THEN
	istr_mant.argumento[3]	=	ls_Columna[1]
END IF

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	'0'

end event

type pb_excel from w_para_informes`pb_excel within w_info_procesoshistoricosunpallet
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_procesoshistoricosunpallet
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_procesoshistoricosunpallet
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_procesoshistoricosunpallet
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_procesoshistoricosunpallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_procesoshistoricosunpallet
integer width = 1902
string text = "Procesos Históricos Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_procesoshistoricosunpallet
integer x = 2281
integer y = 444
integer taborder = 30
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta, li_varirotula
Long		li_planta,li_cliente
String	ls_region

IF Long(istr_mant.argumento[2]) = 0 THEN

		MessageBox("Atención", "Falta Número del Pallet.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()

ELSE
		istr_info.titulo	= 'INFORME PROCESO HISTORICO UN PALLET.'

		OpenWithParm(vinf, istr_info)
		
		IF cbx_varirotula.Checked THEN
			li_varirotula = 1
		ELSE
			li_varirotula = 0
		END IF
		
		vinf.dw_1.DataObject = "dw_info_procesoshistoricosunpalletenc"

		vinf.dw_1.SetTransObject(sqlca)
		
		fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
											Long(istr_mant.Argumento[2]), li_varirotula)
											
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		
		ELSE
			F_Membrete(vinf.dw_1)
			
			vinf.dw_1.Modify("cliente.text = '" + istr_mant.Argumento[3] + "'")
			vinf.dw_1.Modify("pallet.text = '" + istr_mant.Argumento[2] + "'")			
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
			END IF
		END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_procesoshistoricosunpallet
integer x = 2277
integer y = 716
integer taborder = 40
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_4 from statictext within w_info_procesoshistoricosunpallet
integer x = 247
integer y = 440
integer width = 1902
integer height = 236
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_procesoshistoricosunpallet
integer x = 247
integer y = 680
integer width = 1902
integer height = 324
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_procesoshistoricosunpallet
integer x = 311
integer y = 752
integer width = 517
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_procesoshistoricosunpallet
integer x = 887
integer y = 740
integer width = 599
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
long backcolor = 16777215
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[2]	=	This.Text

end event

type dw_2 from datawindow within w_info_procesoshistoricosunpallet
integer x = 887
integer y = 516
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.argumento[3]	=	ls_Columna[1]
	
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_procesoshistoricosunpallet
integer x = 302
integer y = 524
integer width = 233
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

type cb_buscarepa from commandbutton within w_info_procesoshistoricosunpallet
integer x = 1513
integer y = 744
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	istr_mant.argumento[1]
istr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[2] 	=	istr_busq.argum[2]
	em_numero.Text 			=	istr_busq.argum[2]
END IF
end event

type cbx_varirotula from checkbox within w_info_procesoshistoricosunpallet
integer x = 887
integer y = 880
integer width = 626
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
string text = "Variedad Rotulada"
end type

