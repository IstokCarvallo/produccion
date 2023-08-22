$PBExportHeader$w_info_resuplantaporzona.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_resuplantaporzona from w_para_informes
end type
type st_zona from statictext within w_info_resuplantaporzona
end type
type st_14 from statictext within w_info_resuplantaporzona
end type
type st_2 from statictext within w_info_resuplantaporzona
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resuplantaporzona
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resuplantaporzona
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resuplantaporzona
end type
type st_3 from statictext within w_info_resuplantaporzona
end type
type st_44 from statictext within w_info_resuplantaporzona
end type
type sle_1 from singlelineedit within w_info_resuplantaporzona
end type
type cbx_genera from checkbox within w_info_resuplantaporzona
end type
type dw_2 from datawindow within w_info_resuplantaporzona
end type
end forward

global type w_info_resuplantaporzona from w_para_informes
integer x = 14
integer y = 32
integer width = 2418
integer height = 1424
string title = "INFORME -RESUMENL PLANTACIONES POR ZONA"
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
st_44 st_44
sle_1 sle_1
cbx_genera cbx_genera
dw_2 dw_2
end type
global w_info_resuplantaporzona w_info_resuplantaporzona

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	     ii_tipo
String	     is_report, is_nula


end variables

on w_info_resuplantaporzona.create
int iCurrent
call super::create
this.st_zona=create st_zona
this.st_14=create st_14
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.st_44=create st_44
this.sle_1=create sle_1
this.cbx_genera=create cbx_genera
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_zona
this.Control[iCurrent+2]=this.st_14
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.uo_muestrazona
this.Control[iCurrent+5]=this.uo_muestraespecies
this.Control[iCurrent+6]=this.uo_muestravariedad
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_44
this.Control[iCurrent+9]=this.sle_1
this.Control[iCurrent+10]=this.cbx_genera
this.Control[iCurrent+11]=this.dw_2
end on

on w_info_resuplantaporzona.destroy
call super::destroy
destroy(this.st_zona)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.st_44)
destroy(this.sle_1)
destroy(this.cbx_genera)
destroy(this.dw_2)
end on

event open;x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestrazona.Seleccion(True, True)
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
		
   //Zona
	uo_muestrazona.cbx_todos.checked     = True
	uo_muestrazona.cbx_consolida.checked = False
	uo_muestrazona.dw_seleccion.enabled  = True

  	//Especie
	uo_muestraespecies.cbx_todos.checked      = False
	uo_muestraespecies.cbx_todos.visible      = True
	uo_muestraespecies.cbx_consolida.visible  = False
	uo_muestraespecies.cbx_consolida.checked  = False
	uo_muestraespecies.dw_seleccion.enabled   = True
	uo_muestraespecies.dw_seleccion.Object.codigo[1] = 11
	
	//Variedad
	uo_muestravariedad.cbx_todos.checked     = False
	uo_muestravariedad.cbx_consolida.checked = False
	uo_muestravariedad.dw_seleccion.enabled  = True
	uo_muestravariedad.filtra(11)
	uo_muestravariedad.dw_seleccion.Object.codigo[1] = 4
	
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_resuplantaporzona
end type

type st_computador from w_para_informes`st_computador within w_info_resuplantaporzona
end type

type st_usuario from w_para_informes`st_usuario within w_info_resuplantaporzona
end type

type st_temporada from w_para_informes`st_temporada within w_info_resuplantaporzona
end type

type p_logo from w_para_informes`p_logo within w_info_resuplantaporzona
end type

type st_titulo from w_para_informes`st_titulo within w_info_resuplantaporzona
boolean visible = false
integer width = 1664
string text = "Resumen Plantaciones por Zona"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resuplantaporzona
string tag = "Imprimir Reporte"
integer x = 2075
integer y = 520
integer taborder = 60
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_zona, li_especie, li_variedad
String   ls_archivo, ls_ruta

SetPointer(HourGlass!)

//zona

   IF uo_muestrazona.cbx_todos.checked THEN
	   li_zona	= -1
   ELSE
      li_zona	= uo_muestrazona.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_zona)THEN
	      MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Especies
 IF uo_muestraespecies.cbx_todos.checked THEN
	   li_especie	= -1
   ELSE
		li_especie	= uo_muestraespecies.dw_Seleccion.Object.codigo[1]
		IF IsNull(li_especie)THEN
			MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
			RETURN
		END IF
	END IF

//Variedad
IF uo_muestravariedad.cbx_todos.checked THEN
	   li_variedad	= -1
   ELSE
		 li_variedad	= uo_muestravariedad.dw_Seleccion.Object.codigo[1]
			IF IsNull(li_variedad)THEN
				MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
				RETURN
			END IF
END IF

IF cbx_genera.Checked THEN
	dw_2.SetTransObject(SqlCa)
	dw_2.Retrieve(li_especie,li_variedad,li_zona)
	
	ls_Archivo	= '\PLANTACION_POR_ZONA.XLS'
   RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
   	
	IF dw_2.SaveAs(ls_ruta + ls_archivo, Excel! , True)= -1 THEN
		sle_1.text = "Archivo No se pudo generar"
	ELSE
		sle_1.text	= "Archivo generado, " + ls_ruta
	END IF
END IF

istr_info.titulo	= 'INFORME - RESUMEN PLANTACIONES POR ZONA'

OpenWithParm(vinf,istr_info)

  vinf.dw_1.DataObject = "dw_info_plantacionporzona_enca"

vinf.dw_1.SetTransObject(sqlca)
li_fila = vinf.dw_1.Retrieve(li_especie,li_variedad,li_zona)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
   vinf.dw_1.Modify("t_numero.text = '" + '7' + "'")

	
	vinf.dw_1.Object.DataWindow.Zoom = 98
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resuplantaporzona
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2071
integer y = 800
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_zona from statictext within w_info_resuplantaporzona
integer x = 347
integer y = 588
integer width = 325
integer height = 68
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

type st_14 from statictext within w_info_resuplantaporzona
integer x = 347
integer y = 716
integer width = 325
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
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resuplantaporzona
integer x = 1554
integer y = 496
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resuplantaporzona
integer x = 690
integer y = 552
integer width = 1042
integer height = 112
integer taborder = 10
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resuplantaporzona
event destroy ( )
integer x = 690
integer y = 680
integer width = 1056
integer height = 112
integer taborder = 20
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1
		uo_muestravariedad.Todos(True)
		uo_muestravariedad.cbx_Todos.Enabled =	False
		uo_muestravariedad.cbx_Todos.Checked = True
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resuplantaporzona
integer x = 690
integer y = 808
integer width = 1042
integer height = 112
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_resuplantaporzona
integer x = 347
integer y = 848
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

type st_44 from statictext within w_info_resuplantaporzona
integer x = 251
integer y = 440
integer width = 1664
integer height = 720
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

type sle_1 from singlelineedit within w_info_resuplantaporzona
integer x = 270
integer y = 1068
integer width = 1627
integer height = 76
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type cbx_genera from checkbox within w_info_resuplantaporzona
integer x = 347
integer y = 976
integer width = 535
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
string text = "Genera Archivo"
boolean lefttext = true
end type

type dw_2 from datawindow within w_info_resuplantaporzona
boolean visible = false
integer x = 91
integer y = 1216
integer width = 160
integer height = 132
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_plantacionporzona_xls"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

