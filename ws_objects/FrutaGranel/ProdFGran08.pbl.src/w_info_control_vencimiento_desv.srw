$PBExportHeader$w_info_control_vencimiento_desv.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_control_vencimiento_desv from w_para_informes
end type
type st_33 from statictext within w_info_control_vencimiento_desv
end type
type st_14 from statictext within w_info_control_vencimiento_desv
end type
type st_44 from statictext within w_info_control_vencimiento_desv
end type
type st_1 from statictext within w_info_control_vencimiento_desv
end type
type st_3 from statictext within w_info_control_vencimiento_desv
end type
type st_4 from statictext within w_info_control_vencimiento_desv
end type
type st_5 from statictext within w_info_control_vencimiento_desv
end type
type st_6 from statictext within w_info_control_vencimiento_desv
end type
type uo_selplanta from uo_seleccion_plantas within w_info_control_vencimiento_desv
end type
type em_tarja from editmask within w_info_control_vencimiento_desv
end type
type cbx_todostarja from checkbox within w_info_control_vencimiento_desv
end type
type cbx_constarja from checkbox within w_info_control_vencimiento_desv
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_control_vencimiento_desv
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_control_vencimiento_desv
end type
type uo_selespecie from uo_seleccion_especie within w_info_control_vencimiento_desv
end type
type st_2 from statictext within w_info_control_vencimiento_desv
end type
type uo_selproductor from uo_seleccion_productor within w_info_control_vencimiento_desv
end type
type uo_selcategoria from uo_seleccion_categoria within w_info_control_vencimiento_desv
end type
end forward

global type w_info_control_vencimiento_desv from w_para_informes
string tag = "Control de Vencimiento Desverdizado"
integer x = 14
integer y = 32
integer width = 3643
integer height = 1584
string title = "Control Vencimiento Desverdizado"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
boolean clientedge = true
st_33 st_33
st_14 st_14
st_44 st_44
st_1 st_1
st_3 st_3
st_4 st_4
st_5 st_5
st_6 st_6
uo_selplanta uo_selplanta
em_tarja em_tarja
cbx_todostarja cbx_todostarja
cbx_constarja cbx_constarja
uo_selcliente uo_selcliente
uo_selvariedad uo_selvariedad
uo_selespecie uo_selespecie
st_2 st_2
uo_selproductor uo_selproductor
uo_selcategoria uo_selcategoria
end type
global w_info_control_vencimiento_desv w_info_control_vencimiento_desv

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	        is_report, is_nula, is_informe
uo_Especie     iuo_Especie


end variables

on w_info_control_vencimiento_desv.create
int iCurrent
call super::create
this.st_33=create st_33
this.st_14=create st_14
this.st_44=create st_44
this.st_1=create st_1
this.st_3=create st_3
this.st_4=create st_4
this.st_5=create st_5
this.st_6=create st_6
this.uo_selplanta=create uo_selplanta
this.em_tarja=create em_tarja
this.cbx_todostarja=create cbx_todostarja
this.cbx_constarja=create cbx_constarja
this.uo_selcliente=create uo_selcliente
this.uo_selvariedad=create uo_selvariedad
this.uo_selespecie=create uo_selespecie
this.st_2=create st_2
this.uo_selproductor=create uo_selproductor
this.uo_selcategoria=create uo_selcategoria
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_33
this.Control[iCurrent+2]=this.st_14
this.Control[iCurrent+3]=this.st_44
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.uo_selplanta
this.Control[iCurrent+10]=this.em_tarja
this.Control[iCurrent+11]=this.cbx_todostarja
this.Control[iCurrent+12]=this.cbx_constarja
this.Control[iCurrent+13]=this.uo_selcliente
this.Control[iCurrent+14]=this.uo_selvariedad
this.Control[iCurrent+15]=this.uo_selespecie
this.Control[iCurrent+16]=this.st_2
this.Control[iCurrent+17]=this.uo_selproductor
this.Control[iCurrent+18]=this.uo_selcategoria
end on

on w_info_control_vencimiento_desv.destroy
call super::destroy
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_44)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.uo_selplanta)
destroy(this.em_tarja)
destroy(this.cbx_todostarja)
destroy(this.cbx_constarja)
destroy(this.uo_selcliente)
destroy(this.uo_selvariedad)
destroy(this.uo_selespecie)
destroy(this.st_2)
destroy(this.uo_selproductor)
destroy(this.uo_selcategoria)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCategoria.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True, True)
	uo_SelCliente.Seleccion(True, True)
	uo_SelPlanta.Seleccion(True, True)
	uo_SelVariedad.Seleccion(True, True)
	uo_SelCategoria.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	
	uo_SelCliente.Todos(False)
	uo_SelEspecie.Inicia(78)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelVariedad.Filtra(78)
	
	Istr_mant.argumento[1] = "-1"
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_control_vencimiento_desv
integer x = 3589
integer y = 552
end type

type st_computador from w_para_informes`st_computador within w_info_control_vencimiento_desv
integer x = 1536
end type

type st_usuario from w_para_informes`st_usuario within w_info_control_vencimiento_desv
integer x = 1536
end type

type st_temporada from w_para_informes`st_temporada within w_info_control_vencimiento_desv
integer x = 1536
end type

type p_logo from w_para_informes`p_logo within w_info_control_vencimiento_desv
end type

type st_titulo from w_para_informes`st_titulo within w_info_control_vencimiento_desv
integer width = 2784
string text = "Control Vencimiento Desverdizado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_control_vencimiento_desv
string tag = "Imprimir Reporte"
integer x = 3200
integer y = 424
integer taborder = 190
end type

event pb_acepta::clicked;Integer	li_categoria
Long      li_fila, ll_tarja

SetPointer(HourGlass!)

//Tarja
ll_tarja = Long(Istr_mant.argumento[1])
IF ll_tarja = 0 THEN
	MessageBox("Atención","Debe un Número de tarja Previamente",Exclamation!)
	RETURN
END IF

istr_info.titulo	= 'INFORME CONTROL DE VENCIMIENTO DESVERDIZADO'
OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_control_vencimiento_desv"

vinf.dw_1.SetTransObject(sqlca)	

li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,uo_SelEspecie.Codigo, uo_selProductor.Codigo,&
						uo_SelVariedad.Codigo, ll_tarja, uo_SelCategoria.Codigo)
						  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_control_vencimiento_desv
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3200
integer y = 828
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_33 from statictext within w_info_control_vencimiento_desv
integer x = 1723
integer y = 608
integer width = 343
integer height = 84
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_control_vencimiento_desv
integer x = 357
integer y = 1004
integer width = 315
integer height = 84
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_control_vencimiento_desv
integer x = 247
integer y = 424
integer width = 1399
integer height = 956
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_control_vencimiento_desv
integer x = 357
integer y = 608
integer width = 315
integer height = 84
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_control_vencimiento_desv
integer x = 357
integer y = 804
integer width = 315
integer height = 84
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

type st_4 from statictext within w_info_control_vencimiento_desv
integer x = 357
integer y = 1216
integer width = 315
integer height = 84
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_control_vencimiento_desv
integer x = 1723
integer y = 1216
integer width = 343
integer height = 84
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
string text = "Nº Tarja"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_control_vencimiento_desv
integer x = 1714
integer y = 804
integer width = 343
integer height = 84
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
string text = "Categoria"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_info_control_vencimiento_desv
integer x = 635
integer y = 712
integer taborder = 210
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type em_tarja from editmask within w_info_control_vencimiento_desv
integer x = 2080
integer y = 1216
integer width = 288
integer height = 84
integer taborder = 160
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Istr_mant.argumento[1] = this.text
end event

type cbx_todostarja from checkbox within w_info_control_vencimiento_desv
integer x = 2089
integer y = 1124
integer width = 279
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
boolean lefttext = true
end type

event clicked;IF This.Checked THEN 
	istr_mant.argumento[1] = '-1'
	em_tarja.Enabled = False
	em_tarja.text = ""
	cbx_constarja.Checked = False
ELSE
	istr_mant.argumento[1] = ""
	em_tarja.Enabled = True
	em_tarja.Setfocus()
END IF
end event

type cbx_constarja from checkbox within w_info_control_vencimiento_desv
integer x = 2432
integer y = 1124
integer width = 370
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
string text = "Consolida"
boolean lefttext = true
end type

event clicked;IF This.Checked THEN 
	istr_mant.argumento[1] = '-9'
	em_tarja.Enabled = False
	em_tarja.text = ""
	cbx_todostarja.Checked = False
ELSE
	istr_mant.argumento[1] = ""
	em_tarja.Enabled = True
	em_tarja.Setfocus()
END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_control_vencimiento_desv
integer x = 635
integer y = 512
integer taborder = 200
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_control_vencimiento_desv
event destroy ( )
integer x = 635
integer y = 1124
integer taborder = 150
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_control_vencimiento_desv
event destroy ( )
integer x = 635
integer y = 916
integer taborder = 220
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_2 from statictext within w_info_control_vencimiento_desv
integer x = 1646
integer y = 424
integer width = 1399
integer height = 956
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_productor within w_info_control_vencimiento_desv
event destroy ( )
integer x = 2080
integer y = 512
integer taborder = 200
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selcategoria from uo_seleccion_categoria within w_info_control_vencimiento_desv
event destroy ( )
integer x = 2080
integer y = 712
integer taborder = 220
boolean bringtotop = true
end type

on uo_selcategoria.destroy
call uo_seleccion_categoria::destroy
end on

