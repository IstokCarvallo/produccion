$PBExportHeader$w_info_guia_recepcion_def.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_info_guia_recepcion_def from w_para_informes
end type
type st_3 from statictext within w_info_guia_recepcion_def
end type
type st_4 from statictext within w_info_guia_recepcion_def
end type
type pb_1 from picturebutton within w_info_guia_recepcion_def
end type
type st_2 from statictext within w_info_guia_recepcion_def
end type
type em_recepcion from editmask within w_info_guia_recepcion_def
end type
type st_6 from statictext within w_info_guia_recepcion_def
end type
type cbx_1 from checkbox within w_info_guia_recepcion_def
end type
type st_5 from statictext within w_info_guia_recepcion_def
end type
type em_finicio from editmask within w_info_guia_recepcion_def
end type
type em_ftermino from editmask within w_info_guia_recepcion_def
end type
type st_7 from statictext within w_info_guia_recepcion_def
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_guia_recepcion_def
end type
type uo_selplanta from uo_seleccion_plantas within w_info_guia_recepcion_def
end type
end forward

global type w_info_guia_recepcion_def from w_para_informes
integer width = 2597
integer height = 1232
boolean minbox = false
boolean resizable = false
event ue_recuperadatos ( )
event ue_imprimir ( )
st_3 st_3
st_4 st_4
pb_1 pb_1
st_2 st_2
em_recepcion em_recepcion
st_6 st_6
cbx_1 cbx_1
st_5 st_5
em_finicio em_finicio
em_ftermino em_ftermino
st_7 st_7
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_guia_recepcion_def w_info_guia_recepcion_def

type variables
Integer			ii_CuantosLotes

str_busqueda	istr_busq
str_mant			istr_mant
end variables

event ue_recuperadatos();//Datawindowchild  ldwc_lotes
//Long	ll_fila, respuesta
//
//DO
//	dw_lotes.GetChild("vari_codigo", ldwc_lotes)
//	ldwc_lotes.SetTransObject(SqlCa)
//	ldwc_lotes.Retrieve(0)
//
//	dw_lotes.SetTransObject(Sqlca)
//	ll_fila	= dw_lotes.Retrieve(dw_planta.Object.plde_codigo[1],&
//										  Integer(Istr_busq.Argum[2]),&
//										  Integer(istr_busq.Argum[3]),&
//										  dw_cliente.Object.clie_codigo[1])
//										  
//
//	IF ll_fila = -1 THEN
//		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//										Information!, RetryCancel!)
//	ELSEIF ll_fila = 0 THEN
//		Messagebox("Error","No Existe Número de Recepción Para Esta Planta")
//   END IF									
//LOOP WHILE respuesta = 1
//
//IF respuesta = 2 THEN Close(This)
end event

on w_info_guia_recepcion_def.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.pb_1=create pb_1
this.st_2=create st_2
this.em_recepcion=create em_recepcion
this.st_6=create st_6
this.cbx_1=create cbx_1
this.st_5=create st_5
this.em_finicio=create em_finicio
this.em_ftermino=create em_ftermino
this.st_7=create st_7
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.pb_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_recepcion
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cbx_1
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.em_finicio
this.Control[iCurrent+10]=this.em_ftermino
this.Control[iCurrent+11]=this.st_7
this.Control[iCurrent+12]=this.uo_selcliente
this.Control[iCurrent+13]=this.uo_selplanta
end on

on w_info_guia_recepcion_def.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.st_2)
destroy(this.em_recepcion)
destroy(this.st_6)
destroy(this.cbx_1)
destroy(this.st_5)
destroy(this.em_finicio)
destroy(this.em_ftermino)
destroy(this.st_7)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_FInicio.Text		=	String(RelativeDate(Today(), -365), 'dd/mm/yyyy')
	em_FTermino.Text	=	String(RelativeDate(Today(), 1), 'dd/mm/yyyy')
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_guia_recepcion_def
end type

type st_computador from w_para_informes`st_computador within w_info_guia_recepcion_def
integer x = 1024
integer width = 1449
end type

type st_usuario from w_para_informes`st_usuario within w_info_guia_recepcion_def
integer x = 1024
integer width = 1449
end type

type st_temporada from w_para_informes`st_temporada within w_info_guia_recepcion_def
integer x = 1024
integer width = 1449
end type

type p_logo from w_para_informes`p_logo within w_info_guia_recepcion_def
end type

type st_titulo from w_para_informes`st_titulo within w_info_guia_recepcion_def
integer x = 256
integer width = 1769
string text = "Guía Definitiva de Recepción "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_guia_recepcion_def
integer x = 2149
integer y = 364
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		fila
Datawindowchild  ldwc_lotes
Long		ll_fila, respuesta, ll_Numero
Integer	li_imprimio
String 	n_lote, tmp, command
Date 		ld_FechaRecepcionI, ld_FechaRecepcionT

IF (Integer(em_recepcion.text) = 0 OR IsNull(Integer(em_recepcion.text))) AND NOT cbx_1.Checked THEN
	Messagebox("Mensaje","Debe Ingresar Nº Recepción")
	Return 1
ELSE
	ld_FechaRecepcionI	=	Date(em_FInicio.Text)
	ld_FechaRecepcionT	=	Date(em_FTermino.text)
	
	IF cbx_1.Checked THEN
		ll_Numero	=	-1
	ELSE
		ll_Numero	=	Long(em_recepcion.text)
	END IF
	
	OpenWithParm(vinf, istr_info)
	
	vinf.dw_1.DataObject = "dw_info_guia_recepcion_definitiva"
	vinf.dw_1.SetTransObject(Sqlca)

	ll_fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, 1,ll_Numero,uo_SelCliente.Codigo,ld_FechaRecepcionI, ld_FechaRecepcionT)
							  
	IF ll_fila < 1 THEN 
		MessageBox("Error", "No existe información para este informe")
		Return 1
	ELSE
		F_Membrete(vinf.dw_1)
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_guia_recepcion_def
integer x = 2149
integer y = 644
integer taborder = 90
end type

type st_3 from statictext within w_info_guia_recepcion_def
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
long textcolor = 33554431
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_guia_recepcion_def
integer x = 297
integer y = 728
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Nº Recepción"
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_info_guia_recepcion_def
integer x = 1170
integer y = 720
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
long backcolor = 134217748
end type

event clicked;Long	ll_fila

istr_busq.Argum[1]	=	String(uo_SelPlanta.Codigo)
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  '3'
istr_busq.Argum[5]	=  '0'
istr_busq.Argum[4]   =  String(uo_SelCliente.Codigo) //CLIENTE

OpenWithParm(w_busc_spro_recepcion_lote, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[10] = String(1) THEN
	em_recepcion.Text	=	istr_busq.Argum[3]
END IF














end event

type st_2 from statictext within w_info_guia_recepcion_def
integer x = 251
integer y = 404
integer width = 1769
integer height = 596
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

type em_recepcion from editmask within w_info_guia_recepcion_def
integer x = 782
integer y = 720
integer width = 370
integer height = 80
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
string mask = "########"
end type

type st_6 from statictext within w_info_guia_recepcion_def
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
long textcolor = 33554431
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_info_guia_recepcion_def
boolean visible = false
integer x = 1504
integer y = 724
integer width = 402
integer height = 72
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todas"
end type

event clicked;long ll_fila
istr_mant.Argumento[1]	=	' '
istr_mant.Argumento[3] 	= ' '
istr_mant.Argumento[4]	=  ' '

IF This.Checked THEN
	istr_mant.Argumento[1]	=	String(uo_SelPlanta.Codigo)
	istr_mant.Argumento[3]	= '-1'
	istr_mant.Argumento[4]	=  String(uo_SelCliente.Codigo)
	
	em_recepcion.Enabled 	= 	FALSE
	em_recepcion.BackColor	= 	553648127
	pb_1.Enabled				=	FALSE
	This.Checked 				=	TRUE
		
ELSE
	em_recepcion.Enabled 	= 	TRUE
	em_recepcion.BackColor	= 	RGB(255, 255, 255)
	pb_1.Enabled				=	TRUE
	THIS.Checked 				=	FALSE
END IF
end event

type st_5 from statictext within w_info_guia_recepcion_def
integer x = 297
integer y = 856
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Fecha Inicio"
boolean focusrectangle = false
end type

type em_finicio from editmask within w_info_guia_recepcion_def
integer x = 786
integer y = 848
integer width = 443
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_ftermino from editmask within w_info_guia_recepcion_def
integer x = 1509
integer y = 848
integer width = 443
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_7 from statictext within w_info_guia_recepcion_def
integer x = 1266
integer y = 856
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Termino"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_guia_recepcion_def
event destroy ( )
integer x = 777
integer y = 456
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_guia_recepcion_def
event destroy ( )
integer x = 777
integer y = 584
integer height = 100
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

