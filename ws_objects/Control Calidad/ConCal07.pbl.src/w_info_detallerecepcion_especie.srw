$PBExportHeader$w_info_detallerecepcion_especie.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_detallerecepcion_especie from w_para_informes
end type
type dw_1 from uo_dw within w_info_detallerecepcion_especie
end type
type em_fechadesde from editmask within w_info_detallerecepcion_especie
end type
type st_12 from statictext within w_info_detallerecepcion_especie
end type
type st_13 from statictext within w_info_detallerecepcion_especie
end type
type em_fechahasta from editmask within w_info_detallerecepcion_especie
end type
type st_zona from statictext within w_info_detallerecepcion_especie
end type
type st_33 from statictext within w_info_detallerecepcion_especie
end type
type cbx_todosfecha from checkbox within w_info_detallerecepcion_especie
end type
type st_14 from statictext within w_info_detallerecepcion_especie
end type
type st_2 from statictext within w_info_detallerecepcion_especie
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_detallerecepcion_especie
end type
type st_1 from statictext within w_info_detallerecepcion_especie
end type
type uo_muestraplanta from uo_seleccion_planta_mod within w_info_detallerecepcion_especie
end type
type st_5 from statictext within w_info_detallerecepcion_especie
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_detallerecepcion_especie
end type
type st_3 from statictext within w_info_detallerecepcion_especie
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_detallerecepcion_especie
end type
type uo_muestrainspector from uo_seleccion_inspectores_mod within w_info_detallerecepcion_especie
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_detallerecepcion_especie
end type
type st_4 from statictext within w_info_detallerecepcion_especie
end type
type uo_selespecies from uo_seleccion_especie within w_info_detallerecepcion_especie
end type
type gb_3 from groupbox within w_info_detallerecepcion_especie
end type
type st_44 from statictext within w_info_detallerecepcion_especie
end type
end forward

global type w_info_detallerecepcion_especie from w_para_informes
string tag = "Consulta Planilla - Control de Calidad de Recepción"
integer x = 14
integer y = 32
integer width = 2574
integer height = 1984
string title = "CONSULTA PLANILLA  DE RECEPCION"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_14 st_14
st_2 st_2
uo_muestrazona uo_muestrazona
st_1 st_1
uo_muestraplanta uo_muestraplanta
st_5 st_5
uo_muestravariedad uo_muestravariedad
st_3 st_3
uo_muestraproductor uo_muestraproductor
uo_muestrainspector uo_muestrainspector
uo_selcliente uo_selcliente
st_4 st_4
uo_selespecies uo_selespecies
gb_3 gb_3
st_44 st_44
end type
global w_info_detallerecepcion_especie w_info_detallerecepcion_especie

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	    		   is_report, is_nula


end variables

on w_info_detallerecepcion_especie.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.st_1=create st_1
this.uo_muestraplanta=create uo_muestraplanta
this.st_5=create st_5
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.uo_muestraproductor=create uo_muestraproductor
this.uo_muestrainspector=create uo_muestrainspector
this.uo_selcliente=create uo_selcliente
this.st_4=create st_4
this.uo_selespecies=create uo_selespecies
this.gb_3=create gb_3
this.st_44=create st_44
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.em_fechadesde
this.Control[iCurrent+3]=this.st_12
this.Control[iCurrent+4]=this.st_13
this.Control[iCurrent+5]=this.em_fechahasta
this.Control[iCurrent+6]=this.st_zona
this.Control[iCurrent+7]=this.st_33
this.Control[iCurrent+8]=this.cbx_todosfecha
this.Control[iCurrent+9]=this.st_14
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.uo_muestrazona
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.uo_muestraplanta
this.Control[iCurrent+14]=this.st_5
this.Control[iCurrent+15]=this.uo_muestravariedad
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.uo_muestraproductor
this.Control[iCurrent+18]=this.uo_muestrainspector
this.Control[iCurrent+19]=this.uo_selcliente
this.Control[iCurrent+20]=this.st_4
this.Control[iCurrent+21]=this.uo_selespecies
this.Control[iCurrent+22]=this.gb_3
this.Control[iCurrent+23]=this.st_44
end on

on w_info_detallerecepcion_especie.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.st_1)
destroy(this.uo_muestraplanta)
destroy(this.st_5)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.uo_muestraproductor)
destroy(this.uo_muestrainspector)
destroy(this.uo_selcliente)
destroy(this.st_4)
destroy(this.uo_selespecies)
destroy(this.gb_3)
destroy(this.st_44)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecies.Codigo) 			Then lb_Cerrar	=	True
If IsNull(uo_MuestraVariedad.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_MuestraZona.Codigo) 			Then lb_Cerrar	=	True
If IsNull(uo_MuestraProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_MuestraPlanta.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_MuestraInspector.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelCliente.Codigo) 				Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecies.Seleccion(True, False)
	uo_MuestraVariedad.Seleccion(True, False)
	uo_MuestraZona.Seleccion(True, False)
	uo_MuestraProductor.Seleccion(True, False)
	uo_muestraplanta.Seleccion(True, False)
	uo_muestrainspector.Seleccion(True, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelEspecies.Seleccion(False, False)
	
	uo_SelEspecies.Inicia(21)
	uo_SelCliente.Inicia(gi_CodExport)
	
	uo_MuestraInspector.cbx_Todos.Checked = True
	uo_MuestraInspector.Codigo = -1
	uo_MuestraVariedad.Filtra(uo_SelEspecies.Codigo)
	uo_MuestraProductor.Filtra(0)

	uo_Muestraplanta.cbx_Todos.Checked = True
	uo_Muestraplanta.cbx_Todos.TriggerEvent('clicked')
	uo_MuestraInspector.cbx_Todos.TriggerEvent('clicked')
	uo_MuestraProductor.cbx_Todos.TriggerEvent('clicked')
		
	em_FechaDesde.text	  = String(Today())
	em_FechaHasta.text	  = String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_detallerecepcion_especie
boolean visible = true
integer x = 2144
integer y = 532
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;Integer	li_fila, li_consfec
Date		ld_FechaEmbaini, ld_FechaEmbafin
String		ls_Ruta, ls_Archivo

SetPointer(HourGlass!)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
ls_Archivo = '\RecepcionDetallado_' + uo_SelEspecies.Nombre + '.xls'

If cbx_todosfecha.Checked Then
	li_consfec = -1
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
	em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

If uo_SelEspecies.Codigo = 21  Then
	dw_1.DataObject = "dw_info_planillarecep_cerezas_excel"
ElseIf uo_SelEspecies.Codigo = 41  Then
	dw_1.DataObject = "dw_info_planillarecep_kiwis_excel"	
ElseIf uo_SelEspecies.Codigo = 27  Then
	dw_1.DataObject = "dw_info_planillarecep_naranjas_excel"
ElseIf uo_SelEspecies.Codigo= 26  Then
	dw_1.DataObject = "dw_info_planillarecep_clemen_excel"
ElseIf uo_SelEspecies.Codigo = 36 Then
	dw_1.DataObject = "dw_info_planillarecep_mandarina_excel"
ElseIf uo_SelEspecies.Codigo = 81  Then
	dw_1.DataObject = "dw_info_planillarecep_paltas_excel"
ElseIf uo_SelEspecies.Codigo = 78  Then
	dw_1.DataObject = "dw_info_planillarecep_limones_excel"
ElseIf uo_SelEspecies.Codigo = 82  Then
	dw_1.DataObject = "dw_info_planillarecep_granados_excel"
ElseIf uo_SelEspecies.Codigo = 23  Then
	dw_1.DataObject = "dw_info_planillarecep_ciruelas_excel"
Else
	MessageBox( "No Existe información", "No existe información para Especie selecionada.", StopSign!, Ok!)
End If

dw_1.SetTransObject(sqlca)	

li_fila = dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecies.Codigo, uo_MuestraVariedad.Codigo, uo_MuestraZona.Codigo,&
		uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, uo_MuestraInspector.Codigo, ld_FechaEmbaini, ld_FechaEmbafin)
									  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, Excel5!, True) = -1 Then
		MessageBox( "Error", "No se pudo generar archivo.", StopSign!, Ok!)
	Else
		MessageBox( "Atencion", "Se genero archivo: " + ls_Ruta + ls_Archivo, Information!, Ok!)
	End If
End If
	
SetPointer(Arrow!)
end event

type st_computador from w_para_informes`st_computador within w_info_detallerecepcion_especie
end type

type st_usuario from w_para_informes`st_usuario within w_info_detallerecepcion_especie
end type

type st_temporada from w_para_informes`st_temporada within w_info_detallerecepcion_especie
end type

type p_logo from w_para_informes`p_logo within w_info_detallerecepcion_especie
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_detallerecepcion_especie
integer width = 1833
string text = "Consulta Planilla de Recepción"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_detallerecepcion_especie
string tag = "Imprimir Reporte"
integer x = 2121
integer y = 788
integer taborder = 190
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer	li_fila, li_consfec
Date		ld_FechaEmbaini, ld_FechaEmbafin

SetPointer(HourGlass!)

If cbx_todosfecha.Checked Then
	li_consfec = -1
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
	em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

OpenWithParm(vinf,istr_info)

istr_info.titulo	= 'INFORME CONTROL DE CALIDAD DE RECEPCION DE ' + Upper(uo_SelEspecies.Nombre) + ' A PROCESO'

If uo_SelEspecies.Codigo = 21  Then
	vinf.dw_1.DataObject = "dw_info_planillarecep_cerezas"	
	vinf.dw_1.Object.DataWindow.Zoom = 68
ElseIf uo_SelEspecies.Codigo = 41  Then
	vinf.dw_1.DataObject = "dw_info_planillarecep_kiwis"	
	vinf.dw_1.Object.DataWindow.Zoom = 72
ElseIf uo_SelEspecies.Codigo = 27  Then	
	vinf.dw_1.DataObject = "dw_info_planillarecep_naranjas"	
	vinf.dw_1.Object.DataWindow.Zoom = 62
ElseIf uo_SelEspecies.Codigo= 26 Then
	vinf.dw_1.DataObject = "dw_info_planillarecep_clemen_dev"	
	vinf.dw_1.Object.DataWindow.Zoom = 62
ElseIf uo_SelEspecies.Codigo = 36   Then
	vinf.dw_1.DataObject = "dw_info_planillarecep_mandarinas"	
	vinf.dw_1.Object.DataWindow.Zoom = 62
ElseIf uo_SelEspecies.Codigo= 10 Then
	vinf.dw_1.DataObject = "dw_info_planillarecep_lima"	
	vinf.dw_1.Object.DataWindow.Zoom = 90
ElseIf uo_SelEspecies.Codigo = 81  Then	
	vinf.dw_1.DataObject = "dw_info_planillarecep_paltas"
	vinf.dw_1.Object.DataWindow.Zoom = 75
ElseIf uo_SelEspecies.Codigo = 78  Then	
	vinf.dw_1.DataObject = "dw_info_planillarecep_limones"
	vinf.dw_1.Object.DataWindow.Zoom = 63
ElseIf uo_SelEspecies.Codigo = 82  Then	
	vinf.dw_1.DataObject = "dw_info_planillarecep_granados"
	vinf.dw_1.Object.DataWindow.Zoom = 84
ElseIf uo_SelEspecies.Codigo = 23  Then
	vinf.dw_1.DataObject = "dw_info_planillarecep_ciruelas"
	vinf.dw_1.Object.DataWindow.Zoom = 96
Else
	MessageBox( "No Existe información", "No existe información para Especie selecionada.", StopSign!, Ok!)
End If

vinf.dw_1.SetTransObject(sqlca)	
li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecies.Codigo, uo_MuestraVariedad.Codigo, uo_MuestraZona.Codigo,&
			uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, uo_MuestraInspector.Codigo, ld_FechaEmbaini, ld_FechaEmbafin)
										  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_fechahasta.text + "'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_detallerecepcion_especie
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2126
integer y = 1068
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_1 from uo_dw within w_info_detallerecepcion_especie
boolean visible = false
integer x = 2199
integer y = 1632
integer width = 233
integer height = 188
integer taborder = 180
boolean bringtotop = true
boolean vscrollbar = false
end type

event clicked;Integer	li_fila, li_consfec
Date		ld_FechaEmbaini, ld_FechaEmbafin
String		ls_Ruta, ls_Archivo

SetPointer(HourGlass!)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
ls_Archivo = '\RecepcionDetallado_' + uo_SelEspecies.Nombre + '.xls'

If cbx_todosfecha.Checked Then
	li_consfec = -1
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
	em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

If uo_SelEspecies.Codigo = 21  Then
	dw_1.DataObject = "dw_info_planillarecep_cerezas_excel"
ElseIf uo_SelEspecies.Codigo = 41  Then
	dw_1.DataObject = "dw_info_planillarecep_kiwis_excel"	
ElseIf uo_SelEspecies.Codigo = 27  Then
	dw_1.DataObject = "dw_info_planillarecep_naranjas_excel"
ElseIf uo_SelEspecies.Codigo= 26  Then
	dw_1.DataObject = "dw_info_planillarecep_clemen_excel"
ElseIf uo_SelEspecies.Codigo = 36 Then
	dw_1.DataObject = "dw_info_planillarecep_mandarina_excel"
ElseIf uo_SelEspecies.Codigo = 81  Then
	dw_1.DataObject = "dw_info_planillarecep_paltas_excel"
ElseIf uo_SelEspecies.Codigo = 78  Then
	dw_1.DataObject = "dw_info_planillarecep_limones_excel"
ElseIf uo_SelEspecies.Codigo = 82  Then
	dw_1.DataObject = "dw_info_planillarecep_granados_excel"
ElseIf uo_SelEspecies.Codigo = 23  Then
	dw_1.DataObject = "dw_info_planillarecep_ciruelas_excel"
Else
	MessageBox( "No Existe información", "No existe información para Especie selecionada.", StopSign!, Ok!)
End If

dw_1.SetTransObject(sqlca)	

li_fila = dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecies.Codigo, uo_MuestraVariedad.Codigo, uo_MuestraZona.Codigo,&
		uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, uo_MuestraInspector.Codigo, ld_FechaEmbaini, ld_FechaEmbafin)
									  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, Excel5!, True) = -1 Then
		MessageBox( "Error", "No se pudo generar archivo.", StopSign!, Ok!)
	Else
		MessageBox( "Atencion", "Se genero archivo: " + ls_Ruta + ls_Archivo, Information!, Ok!)
	End If
End If
	
SetPointer(Arrow!)
end event

type em_fechadesde from editmask within w_info_detallerecepcion_especie
integer x = 635
integer y = 1600
integer width = 475
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_detallerecepcion_especie
integer x = 439
integer y = 1608
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
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_detallerecepcion_especie
integer x = 1152
integer y = 1612
integer width = 178
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
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_detallerecepcion_especie
integer x = 1344
integer y = 1600
integer width = 475
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_zona from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 1028
integer width = 370
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 1268
integer width = 370
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

type cbx_todosfecha from checkbox within w_info_detallerecepcion_especie
integer x = 1842
integer y = 1612
integer width = 123
integer height = 64
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.enabled   =  TRUE
	//cbx_consfecha.checked   =  FALSE
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0



end event

type st_14 from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 808
integer width = 370
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

type st_2 from statictext within w_info_detallerecepcion_especie
integer x = 1714
integer y = 564
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 1012
integer width = 1024
integer height = 112
integer taborder = 60
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

type st_1 from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 1152
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Frigorífico"
boolean focusrectangle = false
end type

type uo_muestraplanta from uo_seleccion_planta_mod within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 1132
integer width = 1024
integer height = 112
integer taborder = 70
boolean bringtotop = true
end type

on uo_muestraplanta.destroy
call uo_seleccion_planta_mod::destroy
end on

event ue_cambio;call super::ue_cambio;//If IsNull(This.Codigo) Then RETURN
//
//Choose Case This.Codigo
//	Case -1, -9
//		dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
//				
//	Case Else
//		dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
//		
//End Choose
end event

type st_5 from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 1396
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
string text = "Inspector"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 892
integer width = 1024
integer height = 112
integer taborder = 50
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 916
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
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 1252
integer width = 1024
integer height = 112
integer taborder = 80
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then RETURN

Choose Case This.Codigo
	Case -1, -9
		dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
				
	Case Else
		dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
		
End Choose
end event

type uo_muestrainspector from uo_seleccion_inspectores_mod within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 1380
integer width = 1024
integer taborder = 90
boolean bringtotop = true
end type

on uo_muestrainspector.destroy
call uo_seleccion_inspectores_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then RETURN

Choose Case This.Codigo
	Case -1, -9
		dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
				
	Case Else
		dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
		
End Choose
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 684
integer width = 901
integer height = 80
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_4 from statictext within w_info_detallerecepcion_especie
integer x = 425
integer y = 684
integer width = 370
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

type uo_selespecies from uo_seleccion_especie within w_info_detallerecepcion_especie
event destroy ( )
integer x = 864
integer y = 808
integer width = 901
integer height = 80
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecies.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then RETURN

Choose Case This.Codigo
	Case -1, -9
		uo_muestravariedad.Todos(True)
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	Case Else
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
End Choose
end event

type gb_3 from groupbox within w_info_detallerecepcion_especie
integer x = 361
integer y = 1504
integer width = 1595
integer height = 228
integer taborder = 190
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Recepción  "
end type

type st_44 from statictext within w_info_detallerecepcion_especie
integer x = 279
integer y = 504
integer width = 1719
integer height = 1276
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

