$PBExportHeader$w_info_rankingllegada.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_rankingllegada from w_para_informes
end type
type em_desde from editmask within w_info_rankingllegada
end type
type st_12 from statictext within w_info_rankingllegada
end type
type st_13 from statictext within w_info_rankingllegada
end type
type em_hasta from editmask within w_info_rankingllegada
end type
type st_zona from statictext within w_info_rankingllegada
end type
type cbx_todosfecha from checkbox within w_info_rankingllegada
end type
type st_14 from statictext within w_info_rankingllegada
end type
type cbx_consfecha from checkbox within w_info_rankingllegada
end type
type st_2 from statictext within w_info_rankingllegada
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_rankingllegada
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_rankingllegada
end type
type st_1 from statictext within w_info_rankingllegada
end type
type st_4 from statictext within w_info_rankingllegada
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_rankingllegada
end type
type st_3 from statictext within w_info_rankingllegada
end type
type st_8 from statictext within w_info_rankingllegada
end type
type gb_3 from groupbox within w_info_rankingllegada
end type
type st_44 from statictext within w_info_rankingllegada
end type
type uo_muestramercado from uo_seleccion_mercado_mod within w_info_rankingllegada
end type
end forward

global type w_info_rankingllegada from w_para_informes
string tag = "Ranking de llegada por productor"
integer x = 14
integer y = 32
integer width = 2697
integer height = 1824
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_desde em_desde
st_12 st_12
st_13 st_13
em_hasta em_hasta
st_zona st_zona
cbx_todosfecha cbx_todosfecha
st_14 st_14
cbx_consfecha cbx_consfecha
st_2 st_2
uo_muestrazona uo_muestrazona
uo_muestraespecies uo_muestraespecies
st_1 st_1
st_4 st_4
uo_muestravariedad uo_muestravariedad
st_3 st_3
st_8 st_8
gb_3 gb_3
st_44 st_44
uo_muestramercado uo_muestramercado
end type
global w_info_rankingllegada w_info_rankingllegada

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	        is_report, is_nula


end variables

on w_info_rankingllegada.create
int iCurrent
call super::create
this.em_desde=create em_desde
this.st_12=create st_12
this.st_13=create st_13
this.em_hasta=create em_hasta
this.st_zona=create st_zona
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.cbx_consfecha=create cbx_consfecha
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.st_1=create st_1
this.st_4=create st_4
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.st_8=create st_8
this.gb_3=create gb_3
this.st_44=create st_44
this.uo_muestramercado=create uo_muestramercado
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_desde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.cbx_todosfecha
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.cbx_consfecha
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.uo_muestrazona
this.Control[iCurrent+11]=this.uo_muestraespecies
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.uo_muestravariedad
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.st_8
this.Control[iCurrent+17]=this.gb_3
this.Control[iCurrent+18]=this.st_44
this.Control[iCurrent+19]=this.uo_muestramercado
end on

on w_info_rankingllegada.destroy
call super::destroy
destroy(this.em_desde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_hasta)
destroy(this.st_zona)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.cbx_consfecha)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_44)
destroy(this.uo_muestramercado)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_MuestraEspecies.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraVariedad.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraZona.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_MuestraMercado.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestramercado.Seleccion(True, True)
	
	uo_muestraespecies.cbx_todos.checked     = False
	uo_muestraespecies.cbx_todos.visible     	= True
	uo_muestraespecies.cbx_consolida.visible 	= True
	uo_muestraespecies.dw_seleccion.enabled = True
	uo_muestraespecies.dw_seleccion.object.codigo[1] = 11 
	uo_muestravariedad.Filtra(11)
			
	em_Desde.Text	= String(Today())
	em_Hasta.Text		= String(Today())
End If
end event

type st_computador from w_para_informes`st_computador within w_info_rankingllegada
end type

type st_usuario from w_para_informes`st_usuario within w_info_rankingllegada
end type

type st_temporada from w_para_informes`st_temporada within w_info_rankingllegada
end type

type p_logo from w_para_informes`p_logo within w_info_rankingllegada
end type

type st_titulo from w_para_informes`st_titulo within w_info_rankingllegada
integer width = 1833
string text = "Ranking Llegada por Productor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_rankingllegada
string tag = "Imprimir Reporte"
integer x = 2194
integer y = 504
integer taborder = 190
fontcharset fontcharset = ansi!
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Integer	li_fila

OpenWithParm(vinf,istr_info)
istr_info.titulo	= 'INFORME RANKING LLEGADA POR PRODUCTOR'
vinf.dw_1.DataObject = "dw_informe_rankingllegada"

vinf.dw_1.SetTransObject(sqlca)	
li_fila = vinf.dw_1.Retrieve(uo_MuestraZona.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,&
					uo_Muestramercado.Codigo,Date(em_Desde.Text),  Date(em_Hasta.Text))
											  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_Desde.Text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_Hasta.Text + "'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_rankingllegada
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2190
integer y = 784
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_desde from editmask within w_info_rankingllegada
integer x = 745
integer y = 1256
integer width = 338
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_12 from statictext within w_info_rankingllegada
integer x = 517
integer y = 1268
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_rankingllegada
integer x = 1102
integer y = 1268
integer width = 178
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_rankingllegada
integer x = 1275
integer y = 1256
integer width = 338
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_zona from statictext within w_info_rankingllegada
integer x = 306
integer y = 632
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Zona Origen"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_rankingllegada
integer x = 1659
integer y = 1268
integer width = 123
integer height = 64
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = " "
boolean checked = true
end type

event clicked;If This.Checked Then 
	em_Desde.Enabled	=	False
	em_Hasta.Enabled	=	False
	em_Desde.Text		=	'01/01/1900'
	em_Hasta.Text			=	String(Today())
	cbx_consfecha.checked   	=  False
Else
	em_Desde.Enabled	=	True
	em_Hasta.Enabled	=	True
	em_Desde.SetFocus()
End If




end event

type st_14 from statictext within w_info_rankingllegada
integer x = 306
integer y = 772
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type cbx_consfecha from checkbox within w_info_rankingllegada
integer x = 1851
integer y = 1268
integer width = 91
integer height = 64
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = " "
end type

event clicked;IF This.Checked THEN
	em_Desde.Enabled	=	False
	em_Hasta.Enabled	=	False
	em_Desde.Text		=	'01/01/1900'
	em_Hasta.Text			=	String(Today())
	cbx_todosfecha.Checked = False
ELSE
	em_Desde.Enabled	=	True
	em_Hasta.Enabled	=	True
	em_Desde.SetFocus()
END IF

end event

type st_2 from statictext within w_info_rankingllegada
integer x = 1586
integer y = 520
integer width = 197
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_rankingllegada
integer x = 745
integer y = 616
integer width = 1262
integer height = 112
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;//IF IsNull(This.Codigo) THEN RETURN
//
//CHOOSE CASE This.Codigo
//	CASE -1, -9
//		uo_muestrainspector.Todos(True)
//		uo_muestrainspector.cbx_Todos.Enabled	=	False
//		
//	CASE ELSE
//		
//		uo_muestrainspector.Filtra(This.Codigo)
//		uo_muestrainspector.cbx_Todos.Enabled	=	True
//END CHOOSE
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_rankingllegada
event destroy ( )
integer x = 745
integer y = 740
integer width = 1221
integer height = 112
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestravariedad.Todos(True)
			
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
					
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_1 from statictext within w_info_rankingllegada
integer x = 306
integer y = 1028
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Mercado"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_rankingllegada
integer x = 293
integer y = 1168
integer width = 416
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Fecha Arribo"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_rankingllegada
integer x = 745
integer y = 860
integer width = 1230
integer height = 112
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_rankingllegada
integer x = 306
integer y = 896
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Variedad"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_rankingllegada
integer x = 1838
integer y = 520
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cons."
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_rankingllegada
integer x = 293
integer y = 1164
integer width = 1769
integer height = 228
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_44 from statictext within w_info_rankingllegada
integer x = 251
integer y = 440
integer width = 1833
integer height = 1032
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_muestramercado from uo_seleccion_mercado_mod within w_info_rankingllegada
integer x = 750
integer y = 984
integer taborder = 170
boolean bringtotop = true
end type

on uo_muestramercado.destroy
call uo_seleccion_mercado_mod::destroy
end on

