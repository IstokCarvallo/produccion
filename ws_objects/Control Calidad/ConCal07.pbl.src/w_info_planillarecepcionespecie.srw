$PBExportHeader$w_info_planillarecepcionespecie.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_planillarecepcionespecie from w_para_informes
end type
type em_fechadesde from editmask within w_info_planillarecepcionespecie
end type
type st_12 from statictext within w_info_planillarecepcionespecie
end type
type st_13 from statictext within w_info_planillarecepcionespecie
end type
type em_fechahasta from editmask within w_info_planillarecepcionespecie
end type
type st_zona from statictext within w_info_planillarecepcionespecie
end type
type st_33 from statictext within w_info_planillarecepcionespecie
end type
type cbx_todosfecha from checkbox within w_info_planillarecepcionespecie
end type
type st_14 from statictext within w_info_planillarecepcionespecie
end type
type cbx_consfecha from checkbox within w_info_planillarecepcionespecie
end type
type st_2 from statictext within w_info_planillarecepcionespecie
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_planillarecepcionespecie
end type
type st_1 from statictext within w_info_planillarecepcionespecie
end type
type uo_muestraplanta from uo_seleccion_planta_mod within w_info_planillarecepcionespecie
end type
type st_5 from statictext within w_info_planillarecepcionespecie
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_planillarecepcionespecie
end type
type st_3 from statictext within w_info_planillarecepcionespecie
end type
type st_8 from statictext within w_info_planillarecepcionespecie
end type
type gb_3 from groupbox within w_info_planillarecepcionespecie
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_planillarecepcionespecie
end type
type uo_muestrainspector from uo_seleccion_inspectores_mod within w_info_planillarecepcionespecie
end type
type st_44 from statictext within w_info_planillarecepcionespecie
end type
type ddlb_tiporecep from dropdownlistbox within w_info_planillarecepcionespecie
end type
type st_4 from statictext within w_info_planillarecepcionespecie
end type
type cbx_todos from checkbox within w_info_planillarecepcionespecie
end type
type uo_muestraespecies from uo_seleccion_especie within w_info_planillarecepcionespecie
end type
type rb_planta from radiobutton within w_info_planillarecepcionespecie
end type
type rb_productor from radiobutton within w_info_planillarecepcionespecie
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_planillarecepcionespecie
end type
type st_7 from statictext within w_info_planillarecepcionespecie
end type
type gb_4 from groupbox within w_info_planillarecepcionespecie
end type
type st_6 from statictext within w_info_planillarecepcionespecie
end type
end forward

global type w_info_planillarecepcionespecie from w_para_informes
string tag = "Consulta Planilla - Control de Calidad de Recepción"
integer x = 14
integer y = 32
integer width = 2761
integer height = 2376
string title = "CONSULTA PLANILLA  DE RECEPCION"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_14 st_14
cbx_consfecha cbx_consfecha
st_2 st_2
uo_muestrazona uo_muestrazona
st_1 st_1
uo_muestraplanta uo_muestraplanta
st_5 st_5
uo_muestravariedad uo_muestravariedad
st_3 st_3
st_8 st_8
gb_3 gb_3
uo_muestraproductor uo_muestraproductor
uo_muestrainspector uo_muestrainspector
st_44 st_44
ddlb_tiporecep ddlb_tiporecep
st_4 st_4
cbx_todos cbx_todos
uo_muestraespecies uo_muestraespecies
rb_planta rb_planta
rb_productor rb_productor
uo_selcliente uo_selcliente
st_7 st_7
gb_4 gb_4
st_6 st_6
end type
global w_info_planillarecepcionespecie w_info_planillarecepcionespecie

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	    		   is_report, is_nula


end variables

forward prototypes
public subroutine wf_visibilidad (integer ai_especie)
end prototypes

public subroutine wf_visibilidad (integer ai_especie);If ai_Especie = 26 Or ai_Especie = 27  Or ai_Especie = 78 Then
	st_5.visible				=	True
	ddlb_tiporecep.visible	=	True
	cbx_todos.visible		=	True
	gb_4.Visible 			= 	False
	rb_Planta.Visible		=	False
	rb_Productor.Visible	=	False
	st_6.height				=	1180
ElseIf ai_Especie = 21 OR ai_Especie = 41  OR ai_Especie = 23  Then
	gb_4.Visible 				= 	True
	rb_Planta.Visible			= 	True
	rb_Productor.Visible		= 	True
	st_5.visible					=	True
	IF ai_Especie <> 41 THEN
		ddlb_tiporecep.visible	=	True
		cbx_todos.visible		=	True
	ELSE	
		ddlb_tiporecep.visible	=	False
		cbx_todos.visible		=	False
		st_5.visible				=	False
		cbx_todos.visible		=	False
	END IF	
	
	st_6.height				=	1333
Else
	st_5.visible				=	False
	ddlb_tiporecep.visible	=	False
	cbx_todos.visible		=	False
	gb_4.Visible 			= 	False
	rb_Planta.Visible		=	False
	rb_Productor.Visible	=	False
	st_6.height				=	1052
End If

st_44.y					=	st_6.y + st_6.height + 1
gb_3.y					=	st_44.y + 50
st_12.y					=	gb_3.y + 90
st_13.y					=	st_12.y
em_fechadesde.y		=	st_13.y - 8
em_fechahasta.y		=	st_13.y - 8
cbx_todosfecha.y		=	st_13.y + 2
This.Height				=	st_44.Y+ st_44.height + 150
end subroutine

on w_info_planillarecepcionespecie.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.cbx_consfecha=create cbx_consfecha
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.st_1=create st_1
this.uo_muestraplanta=create uo_muestraplanta
this.st_5=create st_5
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.st_8=create st_8
this.gb_3=create gb_3
this.uo_muestraproductor=create uo_muestraproductor
this.uo_muestrainspector=create uo_muestrainspector
this.st_44=create st_44
this.ddlb_tiporecep=create ddlb_tiporecep
this.st_4=create st_4
this.cbx_todos=create cbx_todos
this.uo_muestraespecies=create uo_muestraespecies
this.rb_planta=create rb_planta
this.rb_productor=create rb_productor
this.uo_selcliente=create uo_selcliente
this.st_7=create st_7
this.gb_4=create gb_4
this.st_6=create st_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_14
this.Control[iCurrent+9]=this.cbx_consfecha
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.uo_muestrazona
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.uo_muestraplanta
this.Control[iCurrent+14]=this.st_5
this.Control[iCurrent+15]=this.uo_muestravariedad
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.st_8
this.Control[iCurrent+18]=this.gb_3
this.Control[iCurrent+19]=this.uo_muestraproductor
this.Control[iCurrent+20]=this.uo_muestrainspector
this.Control[iCurrent+21]=this.st_44
this.Control[iCurrent+22]=this.ddlb_tiporecep
this.Control[iCurrent+23]=this.st_4
this.Control[iCurrent+24]=this.cbx_todos
this.Control[iCurrent+25]=this.uo_muestraespecies
this.Control[iCurrent+26]=this.rb_planta
this.Control[iCurrent+27]=this.rb_productor
this.Control[iCurrent+28]=this.uo_selcliente
this.Control[iCurrent+29]=this.st_7
this.Control[iCurrent+30]=this.gb_4
this.Control[iCurrent+31]=this.st_6
end on

on w_info_planillarecepcionespecie.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.cbx_consfecha)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.st_1)
destroy(this.uo_muestraplanta)
destroy(this.st_5)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.uo_muestraproductor)
destroy(this.uo_muestrainspector)
destroy(this.st_44)
destroy(this.ddlb_tiporecep)
destroy(this.st_4)
destroy(this.cbx_todos)
destroy(this.uo_muestraespecies)
destroy(this.rb_planta)
destroy(this.rb_productor)
destroy(this.uo_selcliente)
destroy(this.st_7)
destroy(this.gb_4)
destroy(this.st_6)
end on

event open;call super::open;x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

If IsNull(uo_MuestraEspecies.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraVariedad.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraZona.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_MuestraProductor.Codigo) Then lb_Cerrar=	True
If IsNull(uo_MuestraPlanta.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraInspector.Codigo) Then lb_Cerrar=	True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_MuestraEspecies.Seleccion(False, False)
	uo_MuestraVariedad.Seleccion(True, False)
	uo_MuestraZona.Seleccion(True, False)
	uo_MuestraProductor.Seleccion(True, False)
	uo_MuestraPlanta.Seleccion(True, False)
	uo_MuestraInspector.Seleccion(True, False)
	uo_SelCliente.Seleccion(False, False)
	uo_muestrainspector.cbx_Todos.Checked	=	True

	//Especie
	uo_SelCliente.dw_Seleccion.Object.Codigo[1] = 81
	uo_SelCliente.Codigo = gi_CodExport
	uo_MuestraEspecies.dw_seleccion.object.codigo[1] = 21 
	uo_MuestraEspecies.Codigo = 21
	uo_muestravariedad.Filtra(21)
	wf_visibilidad(21)
	
	em_fechadesde.text	  = String(Today())
	em_fechahasta.text	  = String(Today())

	ii_Tipo = -1
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_planillarecepcionespecie
end type

type st_computador from w_para_informes`st_computador within w_info_planillarecepcionespecie
end type

type st_usuario from w_para_informes`st_usuario within w_info_planillarecepcionespecie
end type

type st_temporada from w_para_informes`st_temporada within w_info_planillarecepcionespecie
end type

type p_logo from w_para_informes`p_logo within w_info_planillarecepcionespecie
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_planillarecepcionespecie
integer width = 1833
string text = "Consulta Planilla de Recepción"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planillarecepcionespecie
string tag = "Imprimir Reporte"
integer x = 2277
integer y = 520
integer taborder = 190
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer	li_fila, li_consfec, li_visible
Date		ld_FechaEmbaini, ld_FechaEmbafin

//Fecha Recepción
If cbx_consfecha.Checked Then
	li_consfec  = -9
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
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
End If

If rb_Planta.Checked Then
	li_visible = 1
Else
	li_visible = 0
End If

OpenWithParm(vinf,istr_info)

If uo_MuestraEspecies.Codigo = 21  Then
	vinf.dw_1.DataObject = "dw_info_planillarecepespenc"
ElseIf uo_MuestraEspecies.Codigo = 41 Then
	vinf.dw_1.DataObject = "dw_info_planillarecepkiwisenc"	
ElseIf uo_MuestraEspecies.Codigo = 27 Or uo_MuestraEspecies.Codigo = 78 Then	
	vinf.dw_1.DataObject = "dw_info_planillarecepnaraenc1"
ElseIf uo_MuestraEspecies.Codigo = 26 Or uo_MuestraEspecies.Codigo = 36 Then	
	vinf.dw_1.DataObject = "dw_info_planillarecepnaraenc_clemen"
ElseIf uo_MuestraEspecies.Codigo = 10 Then	
	vinf.dw_1.DataObject = "dw_info_planillarecepnaraenc_lima"
ElseIf uo_MuestraEspecies.Codigo = 81 Then	
	vinf.dw_1.DataObject = "dw_info_planillareceppaltaenc"
ElseIf uo_MuestraEspecies.Codigo = 23 Then
	vinf.dw_1.DataObject = "dw_info_recepcion_Ciruelas"
//	vinf.dw_1.Modify('DataWindow.Zoom = 90')
ElseIf uo_MuestraEspecies.Codigo = 82 Then
	vinf.dw_1.DataObject	= "dw_info_recepcion_granados"
	vinf.dw_1.ModIfy('DataWindow.Zoom = 95')
Else
	MessageBox( "No Existe información", "No existe información para Especie selecionada.", StopSign!, Ok!)
	Return 
End If

vinf.dw_1.SetTransObject(sqlca)	

If uo_MuestraEspecies.Codigo = 23 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_MuestraPlanta.Codigo, -1, uo_MuestraEspecies.Codigo, uo_MuestraVariedad.Codigo, &
									uo_MuestraZona.Codigo, uo_MuestraProductor.Codigo, uo_MuestraInspector.Codigo, ii_Tipo, &
									ld_FechaEmbaini, ld_FechaEmbafin, li_visible)	
ElseIf uo_MuestraEspecies.Codigo = 82 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_MuestraPlanta.Codigo, -1, uo_MuestraEspecies.Codigo, uo_MuestraVariedad.Codigo, &
									uo_MuestraZona.Codigo, uo_MuestraProductor.Codigo, uo_MuestraInspector.Codigo, ii_Tipo, &
									ld_FechaEmbaini, ld_FechaEmbafin)	
ElseIf uo_MuestraEspecies.Codigo = 41 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_MuestraEspecies.Codigo, uo_MuestraVariedad.Codigo, &
				uo_MuestraZona.Codigo, uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, &
				uo_MuestraInspector.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, -1, li_visible)
Else
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_MuestraEspecies.Codigo, uo_MuestraVariedad.Codigo, &
				uo_MuestraZona.Codigo, uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, &
				uo_MuestraInspector.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, ii_Tipo, -1, li_visible)
End If
				
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

type pb_salir from w_para_informes`pb_salir within w_info_planillarecepcionespecie
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2281
integer y = 800
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_planillarecepcionespecie
integer x = 571
integer y = 1884
integer width = 485
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

type st_12 from statictext within w_info_planillarecepcionespecie
integer x = 361
integer y = 1896
integer width = 215
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

type st_13 from statictext within w_info_planillarecepcionespecie
integer x = 1074
integer y = 1896
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

type em_fechahasta from editmask within w_info_planillarecepcionespecie
integer x = 1271
integer y = 1884
integer width = 471
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

type st_zona from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 896
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

type st_33 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 1136
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

type cbx_todosfecha from checkbox within w_info_planillarecepcionespecie
integer x = 1783
integer y = 1892
integer width = 123
integer height = 64
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
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
	cbx_consfecha.checked   =  FALSE
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0



end event

type st_14 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 548
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

type cbx_consfecha from checkbox within w_info_planillarecepcionespecie
boolean visible = false
integer x = 1920
integer y = 1892
integer width = 91
integer height = 64
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = " "
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.enabled   =  TRUE
	cbx_todosfecha.Checked = False
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0
end event

type st_2 from statictext within w_info_planillarecepcionespecie
integer x = 1614
integer y = 488
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_planillarecepcionespecie
integer x = 763
integer y = 884
integer width = 1211
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

type st_1 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 1020
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

type uo_muestraplanta from uo_seleccion_planta_mod within w_info_planillarecepcionespecie
integer x = 763
integer y = 1000
integer width = 1211
integer height = 112
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraplanta.destroy
call uo_seleccion_planta_mod::destroy
end on

type st_5 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 1376
integer width = 466
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
string text = "Tipo Recepción"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_planillarecepcionespecie
integer x = 763
integer y = 760
integer width = 1211
integer height = 112
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 784
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

type st_8 from statictext within w_info_planillarecepcionespecie
boolean visible = false
integer x = 1865
integer y = 488
integer width = 201
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
string text = "Cons."
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_planillarecepcionespecie
integer x = 293
integer y = 1800
integer width = 1769
integer height = 228
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Recepción "
end type

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_planillarecepcionespecie
event destroy ( )
integer x = 763
integer y = 1116
integer width = 1211
integer height = 112
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_muestrainspector from uo_seleccion_inspectores_mod within w_info_planillarecepcionespecie
event destroy ( )
integer x = 763
integer y = 1232
integer width = 1211
integer height = 112
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestrainspector.destroy
call uo_seleccion_inspectores_mod::destroy
end on

type st_44 from statictext within w_info_planillarecepcionespecie
integer x = 251
integer y = 1752
integer width = 1833
integer height = 324
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

type ddlb_tiporecep from dropdownlistbox within w_info_planillarecepcionespecie
integer x = 773
integer y = 1360
integer width = 873
integer height = 400
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean sorted = false
string item[] = {"Huerto","Pre Proceso","Post Desverdizado","Post Almacenaje"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tipo	=	Index
end event

type st_4 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 1264
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

type cbx_todos from checkbox within w_info_planillarecepcionespecie
integer x = 1682
integer y = 1372
integer width = 91
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	ddlb_tiporecep.Enabled	=	False
	ii_Tipo = -1
Else
	ddlb_tiporecep.Enabled	=	True
	ddlb_tiporecep.Selectitem(1)
	ii_Tipo = 1
End If
end event

type uo_muestraespecies from uo_seleccion_especie within w_info_planillarecepcionespecie
integer x = 763
integer y = 668
integer height = 80
integer taborder = 200
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_MuestraVariedad.Filtra(This.Codigo)
		wf_visibilidad(This.Codigo)
		
End Choose
end event

on uo_muestraespecies.destroy
call uo_seleccion_especie::destroy
end on

type rb_planta from radiobutton within w_info_planillarecepcionespecie
integer x = 594
integer y = 1592
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean checked = true
end type

type rb_productor from radiobutton within w_info_planillarecepcionespecie
integer x = 1202
integer y = 1592
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_planillarecepcionespecie
integer x = 763
integer y = 548
integer height = 80
integer taborder = 170
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_7 from statictext within w_info_planillarecepcionespecie
integer x = 288
integer y = 668
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

type gb_4 from groupbox within w_info_planillarecepcionespecie
integer x = 293
integer y = 1504
integer width = 1769
integer height = 228
integer taborder = 170
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Tipo de Informe "
end type

type st_6 from statictext within w_info_planillarecepcionespecie
integer x = 251
integer y = 440
integer width = 1833
integer height = 1308
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

