$PBExportHeader$w_info_totalplantaporano.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_totalplantaporano from w_para_informes
end type
type st_zona from statictext within w_info_totalplantaporano
end type
type st_14 from statictext within w_info_totalplantaporano
end type
type st_2 from statictext within w_info_totalplantaporano
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_totalplantaporano
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_totalplantaporano
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_totalplantaporano
end type
type st_3 from statictext within w_info_totalplantaporano
end type
type em_desde from editmask within w_info_totalplantaporano
end type
type gb_3 from groupbox within w_info_totalplantaporano
end type
type st_1 from statictext within w_info_totalplantaporano
end type
type st_4 from statictext within w_info_totalplantaporano
end type
type pb_1 from picturebutton within w_info_totalplantaporano
end type
type dw_2 from datawindow within w_info_totalplantaporano
end type
type sle_1 from singlelineedit within w_info_totalplantaporano
end type
type st_44 from statictext within w_info_totalplantaporano
end type
type ddlb_grafico from dropdownlistbox within w_info_totalplantaporano
end type
type st_5 from statictext within w_info_totalplantaporano
end type
type em_hasta from editmask within w_info_totalplantaporano
end type
end forward

global type w_info_totalplantaporano from w_para_informes
integer x = 14
integer y = 32
integer width = 2747
integer height = 1688
string title = "INFORME -TOTAL PLANTACIONES POR AÑO"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
st_zona st_zona
st_14 st_14
st_2 st_2
uo_muestrazona uo_muestrazona
uo_muestraespecies uo_muestraespecies
uo_muestravariedad uo_muestravariedad
st_3 st_3
em_desde em_desde
gb_3 gb_3
st_1 st_1
st_4 st_4
pb_1 pb_1
dw_2 dw_2
sle_1 sle_1
st_44 st_44
ddlb_grafico ddlb_grafico
st_5 st_5
em_hasta em_hasta
end type
global w_info_totalplantaporano w_info_totalplantaporano

type variables
str_busqueda      istr_busq
str_mant          istr_mant
Integer	     ii_tipo,ii_imprime, ii_genera, ii_tipograf
String	     is_report, is_nula



end variables

on w_info_totalplantaporano.create
int iCurrent
call super::create
this.st_zona=create st_zona
this.st_14=create st_14
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.em_desde=create em_desde
this.gb_3=create gb_3
this.st_1=create st_1
this.st_4=create st_4
this.pb_1=create pb_1
this.dw_2=create dw_2
this.sle_1=create sle_1
this.st_44=create st_44
this.ddlb_grafico=create ddlb_grafico
this.st_5=create st_5
this.em_hasta=create em_hasta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_zona
this.Control[iCurrent+2]=this.st_14
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.uo_muestrazona
this.Control[iCurrent+5]=this.uo_muestraespecies
this.Control[iCurrent+6]=this.uo_muestravariedad
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.em_desde
this.Control[iCurrent+9]=this.gb_3
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.pb_1
this.Control[iCurrent+13]=this.dw_2
this.Control[iCurrent+14]=this.sle_1
this.Control[iCurrent+15]=this.st_44
this.Control[iCurrent+16]=this.ddlb_grafico
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.em_hasta
end on

on w_info_totalplantaporano.destroy
call super::destroy
destroy(this.st_zona)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.gb_3)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.dw_2)
destroy(this.sle_1)
destroy(this.st_44)
destroy(this.ddlb_grafico)
destroy(this.st_5)
destroy(this.em_hasta)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestrazona.Seleccion(True, True)
	uo_muestraespecies.Seleccion(False, False)
	uo_muestravariedad.Seleccion(True, True)

  	//Especie
	uo_muestraespecies.dw_seleccion.Object.codigo[1] = 11
	uo_muestraespecies.Codigo								= 11
	
	uo_muestravariedad.Filtra(uo_muestraespecies.Codigo)
	
	em_desde.text = String(year(today()) - 1)
	em_hasta.text = String(year(today()))
	ii_imprime = 0
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_totalplantaporano
boolean visible = true
integer x = 2071
integer y = 404
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(Arrow!)

Integer	li_fila, li_desde, li_hasta 
String 	ls_ruta, ls_archivo

SetPointer(HourGlass!)

li_desde = Integer(em_desde.text)
li_hasta = Integer(em_hasta.text)

dw_2.SetTransObject(SqlCa)
dw_2.Retrieve(uo_MuestraZona.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,li_desde,li_hasta)

ls_Archivo	= '\PLANTACION_POR_AÑO.XLS'
RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	
IF dw_2.SaveAs(ls_ruta + ls_archivo, Excel! , True)= -1 THEN
	sle_1.text = "Archivo No se pudo generar"
ELSE
	sle_1.text	= "Archivo generado, " + ls_ruta + ls_archivo
END IF

SetPointer(Arrow!)
end event

type st_computador from w_para_informes`st_computador within w_info_totalplantaporano
end type

type st_usuario from w_para_informes`st_usuario within w_info_totalplantaporano
end type

type st_temporada from w_para_informes`st_temporada within w_info_totalplantaporano
end type

type p_logo from w_para_informes`p_logo within w_info_totalplantaporano
end type

type st_titulo from w_para_informes`st_titulo within w_info_totalplantaporano
integer width = 1664
integer height = 84
string text = "Plantaciones por Año"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_totalplantaporano
string tag = "Imprimir Reporte"
integer x = 2075
integer y = 692
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_desde, li_hasta

SetPointer(HourGlass!)
 	
li_desde = Integer(em_desde.text)
li_hasta = Integer(em_hasta.text)

istr_info.titulo	= 'INFORME - PLANTACIONES POR AÑO'

OpenWithParm(vinf,istr_info)

IF ii_Imprime > 0 THEN
	 vinf.dw_1.DataObject = "dw_info_grafplantaporano"
	 vinf.dw_1.Object.gr_1.graphtype = ii_tipograf
ELSE 
 	vinf.dw_1.DataObject = "dw_info_plantacionporano_enca"	
END IF

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(uo_MuestraZona.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,li_desde,li_hasta)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_numero.text = '" + '6' + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_totalplantaporano
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2071
integer y = 960
integer taborder = 100
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_zona from statictext within w_info_totalplantaporano
integer x = 352
integer y = 560
integer width = 325
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
string text = "Zona"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_totalplantaporano
integer x = 352
integer y = 688
integer width = 325
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

type st_2 from statictext within w_info_totalplantaporano
integer x = 1600
integer y = 468
integer width = 197
integer height = 60
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
string text = "Todos"
boolean focusrectangle = false
end type

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_totalplantaporano
integer x = 736
integer y = 524
integer width = 1010
integer height = 112
integer taborder = 20
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_totalplantaporano
event destroy ( )
integer x = 736
integer y = 652
integer width = 905
integer height = 112
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1
		uo_muestravariedad.Todos(True)
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_totalplantaporano
integer x = 736
integer y = 784
integer width = 1010
integer height = 112
integer taborder = 40
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_totalplantaporano
integer x = 352
integer y = 820
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
string text = "Variedad"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_totalplantaporano
integer x = 736
integer y = 1036
integer width = 265
integer height = 80
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
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

type gb_3 from groupbox within w_info_totalplantaporano
integer x = 306
integer y = 924
integer width = 1536
integer height = 232
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Año Plantación"
end type

type st_1 from statictext within w_info_totalplantaporano
integer x = 375
integer y = 1044
integer width = 219
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
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_totalplantaporano
integer x = 1093
integer y = 1044
integer width = 206
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
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_info_totalplantaporano
integer x = 1623
integer y = 1164
integer width = 151
integer height = 124
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string picturename = "CheckStatus5!"
alignment htextalign = left!
long backcolor = 33543637
end type

event clicked;IF IsNull(ii_Tipograf) OR ii_TipoGraf = 0 THEN
	MessageBox("","SELECCIONE GRAFICO") 
ELSE
	ii_Imprime = 1
	pb_acepta.TriggerEvent(Clicked!)
END IF
end event

type dw_2 from datawindow within w_info_totalplantaporano
boolean visible = false
integer x = 1970
integer y = 224
integer width = 169
integer height = 152
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_plantacionporano_xls"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type sle_1 from singlelineedit within w_info_totalplantaporano
integer x = 256
integer y = 1340
integer width = 1664
integer height = 112
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 65535
long backcolor = 16711680
borderstyle borderstyle = stylelowered!
end type

type st_44 from statictext within w_info_totalplantaporano
integer x = 251
integer y = 440
integer width = 1664
integer height = 892
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type ddlb_grafico from dropdownlistbox within w_info_totalplantaporano
integer x = 736
integer y = 1180
integer width = 869
integer height = 404
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 16777215
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Area","Bar","Bar3D","Bar3DObj","BarStacked","BarStacked3DObj","Col","Col3D","Col3DObj","ColStacked","ColStacked3DObj","Line","Pie","Scatter","Area3D","Line3D","Pie3D"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tipograf = index

end event

type st_5 from statictext within w_info_totalplantaporano
integer x = 352
integer y = 1196
integer width = 370
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
string text = "Tipo Gráfico"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_totalplantaporano
integer x = 1335
integer y = 1036
integer width = 265
integer height = 80
integer taborder = 60
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
string mask = "####"
end type

