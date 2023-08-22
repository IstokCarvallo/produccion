$PBExportHeader$w_info_planilla_identifica_lote.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_info_planilla_identifica_lote from w_para_informes
end type
type st_3 from statictext within w_info_planilla_identifica_lote
end type
type st_4 from statictext within w_info_planilla_identifica_lote
end type
type pb_1 from picturebutton within w_info_planilla_identifica_lote
end type
type rb_2 from radiobutton within w_info_planilla_identifica_lote
end type
type gb_3 from groupbox within w_info_planilla_identifica_lote
end type
type st_5 from statictext within w_info_planilla_identifica_lote
end type
type rb_1 from radiobutton within w_info_planilla_identifica_lote
end type
type dw_lotes from datawindow within w_info_planilla_identifica_lote
end type
type st_2 from statictext within w_info_planilla_identifica_lote
end type
type em_recepcion from editmask within w_info_planilla_identifica_lote
end type
type st_6 from statictext within w_info_planilla_identifica_lote
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_planilla_identifica_lote
end type
type uo_selplantas from uo_seleccion_plantas within w_info_planilla_identifica_lote
end type
end forward

global type w_info_planilla_identifica_lote from w_para_informes
integer width = 3264
integer height = 2072
boolean resizable = false
event ue_recuperadatos ( )
event ue_imprimir ( )
st_3 st_3
st_4 st_4
pb_1 pb_1
rb_2 rb_2
gb_3 gb_3
st_5 st_5
rb_1 rb_1
dw_lotes dw_lotes
st_2 st_2
em_recepcion em_recepcion
st_6 st_6
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_info_planilla_identifica_lote w_info_planilla_identifica_lote

type variables
str_busqueda	istr_busq
str_mant			istr_mant
end variables

event ue_recuperadatos();Datawindowchild  ldwc_lotes
Long	ll_fila, respuesta

DO
	dw_lotes.GetChild("vari_codigo", ldwc_lotes)
	ldwc_lotes.SetTransObject(SqlCa)
	ldwc_lotes.Retrieve(0)

	dw_lotes.SetTransObject(Sqlca)
	ll_fila	= dw_lotes.Retrieve(uo_SelPlantas.Codigo,Integer(Istr_busq.Argum[2]),&
										  Integer(em_recepcion.Text), uo_SelCliente.Codigo)
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila = 0 THEN
		Messagebox("Error","No Existe Número de Recepción Para Esta Planta")
   END IF		
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_info_planilla_identifica_lote.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.pb_1=create pb_1
this.rb_2=create rb_2
this.gb_3=create gb_3
this.st_5=create st_5
this.rb_1=create rb_1
this.dw_lotes=create dw_lotes
this.st_2=create st_2
this.em_recepcion=create em_recepcion
this.st_6=create st_6
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.pb_1
this.Control[iCurrent+4]=this.rb_2
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.rb_1
this.Control[iCurrent+8]=this.dw_lotes
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.em_recepcion
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.uo_selcliente
this.Control[iCurrent+13]=this.uo_selplantas
end on

on w_info_planilla_identifica_lote.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.rb_2)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.rb_1)
destroy(this.dw_lotes)
destroy(this.st_2)
destroy(this.em_recepcion)
destroy(this.st_6)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gstr_paramPlanta.CodigoPlanta)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_planilla_identifica_lote
integer x = 3465
integer y = 788
end type

type st_computador from w_para_informes`st_computador within w_info_planilla_identifica_lote
integer x = 1189
integer width = 1737
end type

type st_usuario from w_para_informes`st_usuario within w_info_planilla_identifica_lote
integer x = 1189
integer width = 1737
end type

type st_temporada from w_para_informes`st_temporada within w_info_planilla_identifica_lote
integer x = 1189
integer width = 1737
end type

type p_logo from w_para_informes`p_logo within w_info_planilla_identifica_lote
end type

type st_titulo from w_para_informes`st_titulo within w_info_planilla_identifica_lote
integer width = 2455
string text = "Planilla Identificatoria De Lotes (Tarjas)"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planilla_identifica_lote
integer x = 2880
integer y = 552
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		fila
Datawindowchild  ldwc_lotes
Long		ll_fila, respuesta
Integer	li_imprimio
String 	n_lote, tmp, command

	IF Integer(em_recepcion.text) = 0 OR IsNull(Integer(em_recepcion.text)) THEN
      Messagebox("Mensaje","Debe Ingresar Nº Recepción")
	   Return 1
	ELSE
		OpenWithParm(vinf, istr_info)
		vinf.dw_1.DataObject = "dw_info_lotesfrutagranel_recepcion_grand"
	
	DO
		vinf.dw_1.GetChild("vari_codigo", ldwc_lotes)
		ldwc_lotes.SetTransObject(SqlCa)
		ldwc_lotes.Retrieve(uo_SelCliente.Codigo)
		vinf.dw_1.SetTransObject(Sqlca)
		ll_fila	= vinf.dw_1.Retrieve(uo_SelPLantas.Codigo, uo_SelCliente.Codigo,Integer(Istr_busq.Argum[2]),&
												Integer(em_recepcion.text) )
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		ELSE
			F_Membrete(vinf.dw_1)
		
			FOR ll_fila = 1 TO dw_lotes.RowCount()
				IF dw_lotes.IsSelected(ll_fila) THEN
					n_lote =  String(dw_lotes.object.compute_2[ll_fila])
					vinf.dw_1.SetFilter("compute_2 = '"+ n_lote +" '")
					vinf.dw_1.filter()
					vinf.dw_1.Print()
				END IF	
			NEXT	
			SetPointer(Arrow!)
		END IF
	LOOP WHILE respuesta = 1
  END IF


end event

type pb_salir from w_para_informes`pb_salir within w_info_planilla_identifica_lote
integer x = 2880
integer y = 916
integer taborder = 80
end type

type st_3 from statictext within w_info_planilla_identifica_lote
integer x = 297
integer y = 600
integer width = 402
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_planilla_identifica_lote
integer x = 297
integer y = 716
integer width = 402
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
string text = "Nº Recepción"
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_info_planilla_identifica_lote
integer x = 1189
integer y = 700
integer width = 101
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
boolean originalsize = true
end type

event clicked;Long	ll_fila

istr_busq.Argum[1]	=	String(uo_SelPlantas.Codigo)
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  '3'
istr_busq.Argum[4]   =  '-1'
istr_busq.Argum[5]	=  '0'
//istr_busq.Argum[4]   =  String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_spro_recepcion_lote, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[10] = String(1) THEN
	em_recepcion.Text	=	istr_busq.Argum[3]
	Parent.TriggerEvent("ue_recuperadatos")

	FOR ll_fila	=	1 TO dw_lotes.RowCount()
		dw_lotes.SelectRow(ll_fila, TRUE)
	NEXT 

	IF ll_fila	>	1 THEN
 		rb_1.Checked = TRUE
	END IF 
END IF
end event

type rb_2 from radiobutton within w_info_planilla_identifica_lote
integer x = 1335
integer y = 944
integer width = 622
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desaplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, False)
NEXT 
end event

type gb_3 from groupbox within w_info_planilla_identifica_lote
integer x = 297
integer y = 852
integer width = 2382
integer height = 216
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Opciones"
borderstyle borderstyle = styleraised!
end type

type st_5 from statictext within w_info_planilla_identifica_lote
integer x = 251
integer y = 828
integer width = 2455
integer height = 1060
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

type rb_1 from radiobutton within w_info_planilla_identifica_lote
integer x = 635
integer y = 944
integer width = 622
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Aplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, True)
NEXT 
end event

type dw_lotes from datawindow within w_info_planilla_identifica_lote
integer x = 288
integer y = 1092
integer width = 2382
integer height = 748
boolean bringtotop = true
boolean titlebar = true
string title = "Lotes"
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event clicked;
IF IsSelected(Row) THEN
	SelectRow (Row, FALSE)
ELSE
	SelectRow (Row, TRUE)
END IF


end event

type st_2 from statictext within w_info_planilla_identifica_lote
integer x = 251
integer y = 404
integer width = 2455
integer height = 420
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

type em_recepcion from editmask within w_info_planilla_identifica_lote
integer x = 777
integer y = 700
integer width = 370
integer height = 80
integer taborder = 30
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
string mask = "########"
end type

event modified;long ll_fila

istr_busq.Argum[1]	=	' '
istr_busq.Argum[2]	=  ' '
istr_busq.Argum[3]	=  ' '
istr_busq.Argum[5]	=  ' '

istr_busq.Argum[1]	=	String(uo_SelPlantas.Codigo)
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  em_recepcion.Text
istr_busq.Argum[5]	=  '0'
istr_busq.Argum[4]	=  String(uo_SelCliente.Codigo)

Parent.TriggerEvent("ue_recuperadatos")

FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, TRUE)
NEXT 

IF ll_fila	>	1 THEN
 rb_1.Checked = True
END IF 

end event

type st_6 from statictext within w_info_planilla_identifica_lote
integer x = 297
integer y = 472
integer width = 251
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_planilla_identifica_lote
event destroy ( )
integer x = 786
integer y = 460
integer height = 92
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_planilla_identifica_lote
event destroy ( )
integer x = 786
integer y = 588
integer height = 92
integer taborder = 80
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

