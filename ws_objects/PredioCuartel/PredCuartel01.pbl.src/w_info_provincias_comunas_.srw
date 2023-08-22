$PBExportHeader$w_info_provincias_comunas_.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_provincias_comunas_ from w_para_informes
end type
type st_zona from statictext within w_info_provincias_comunas_
end type
type st_33 from statictext within w_info_provincias_comunas_
end type
type st_14 from statictext within w_info_provincias_comunas_
end type
type st_2 from statictext within w_info_provincias_comunas_
end type
type st_1 from statictext within w_info_provincias_comunas_
end type
type st_44 from statictext within w_info_provincias_comunas_
end type
type st_5 from statictext within w_info_provincias_comunas_
end type
type uo_muestraprovincias from uo_seleccion_provincias_mod within w_info_provincias_comunas_
end type
type uo_muestracomunas from uo_seleccion_comunasexp_mod within w_info_provincias_comunas_
end type
type ddlb_regiones from dropdownlistbox within w_info_provincias_comunas_
end type
type uo_muestraciudades from uo_seleccion_ciudades_mod within w_info_provincias_comunas_
end type
type cbx_todos from checkbox within w_info_provincias_comunas_
end type
end forward

global type w_info_provincias_comunas_ from w_para_informes
integer x = 14
integer y = 32
integer width = 2281
integer height = 1028
string title = "INFORME - PROVINCIAS COMUNAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
//string icon = "\Desarrollo\Produccion\FrutaProcesada\Producc.ico"
st_zona st_zona
st_33 st_33
st_14 st_14
st_2 st_2
st_1 st_1
st_44 st_44
st_5 st_5
uo_muestraprovincias uo_muestraprovincias
uo_muestracomunas uo_muestracomunas
ddlb_regiones ddlb_regiones
uo_muestraciudades uo_muestraciudades
cbx_todos cbx_todos
end type
global w_info_provincias_comunas_ w_info_provincias_comunas_

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	     ii_tipo
String	     is_report, is_nula


end variables

on w_info_provincias_comunas_.create
int iCurrent
call super::create
this.st_zona=create st_zona
this.st_33=create st_33
this.st_14=create st_14
this.st_2=create st_2
this.st_1=create st_1
this.st_44=create st_44
this.st_5=create st_5
this.uo_muestraprovincias=create uo_muestraprovincias
this.uo_muestracomunas=create uo_muestracomunas
this.ddlb_regiones=create ddlb_regiones
this.uo_muestraciudades=create uo_muestraciudades
this.cbx_todos=create cbx_todos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_zona
this.Control[iCurrent+2]=this.st_33
this.Control[iCurrent+3]=this.st_14
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_44
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.uo_muestraprovincias
this.Control[iCurrent+9]=this.uo_muestracomunas
this.Control[iCurrent+10]=this.ddlb_regiones
this.Control[iCurrent+11]=this.uo_muestraciudades
this.Control[iCurrent+12]=this.cbx_todos
end on

on w_info_provincias_comunas_.destroy
call super::destroy
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_44)
destroy(this.st_5)
destroy(this.uo_muestraprovincias)
destroy(this.uo_muestracomunas)
destroy(this.ddlb_regiones)
destroy(this.uo_muestraciudades)
destroy(this.cbx_todos)
end on

event open;x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

IF IsNull(uo_muestraprovincias.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraciudades.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestracomunas.Codigo) THEN lb_Cerrar	=	True


IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestraprovincias.Seleccion(True, True)
	uo_muestraciudades.Seleccion(True, True)
	uo_muestracomunas.Seleccion(True, True)
		
	cbx_todos.Checked = True
	ddlb_regiones.Enabled = False
   
  //Provincias
	uo_muestraprovincias.cbx_consolida.checked = False
	uo_muestraprovincias.cbx_todos.checked     = True
	uo_muestraprovincias.dw_seleccion.enabled  = True
	uo_muestraprovincias.filtra(0)
	
	//Ciudades
	uo_muestraciudades.cbx_todos.checked     = True
	uo_muestraciudades.cbx_consolida.checked = False
	uo_muestraciudades.dw_seleccion.enabled  = True
	uo_muestraciudades.filtra(0,0)
	
	//Comunas
	uo_muestracomunas.cbx_todos.checked      = True
	uo_muestracomunas.cbx_consolida.checked  = False
	uo_muestracomunas.dw_seleccion.enabled   = True
	uo_muestracomunas.filtra(0,0,0)
		
END IF
end event

type st_computador from w_para_informes`st_computador within w_info_provincias_comunas_
end type

type st_usuario from w_para_informes`st_usuario within w_info_provincias_comunas_
end type

type st_temporada from w_para_informes`st_temporada within w_info_provincias_comunas_
end type

type p_logo from w_para_informes`p_logo within w_info_provincias_comunas_
end type

type st_titulo from w_para_informes`st_titulo within w_info_provincias_comunas_
boolean visible = false
integer x = 37
integer y = 64
integer width = 1833
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_provincias_comunas_
string tag = "Imprimir Reporte"
integer x = 2002
integer y = 228
integer taborder = 190
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_comuna, li_ciudad, li_provincias  

SetPointer(HourGlass!)

//Regiones
IF cbx_todos.Checked THEN
	istr_mant.argumento[1] = '0'
ELSE
	IF IsNull(istr_mant.argumento[1]) OR istr_mant.argumento[1] = "" THEN
		MessageBox("Atención","Debe Seleccionar una Región Previamente",Exclamation!)
		RETURN
	END IF
END IF
//Provincias
   IF uo_muestraprovincias.cbx_todos.checked THEN
	   li_provincias = 0
   ELSE
      li_provincias = uo_muestraprovincias.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_provincias)THEN
	      MessageBox("Atención","Debe Seleccionar una Provincia Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Ciudades
   IF uo_muestraciudades.cbx_todos.checked THEN
	   li_ciudad = 0
   ELSE
      li_ciudad = uo_muestraciudades.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_ciudad)THEN
	      MessageBox("Atención","Debe Seleccionar una Ciudad Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Comunas
IF uo_muestracomunas.cbx_todos.checked THEN
	li_comuna = 0
ELSE
	li_comuna	= uo_muestracomunas.dw_Seleccion.Object.codigo[1]
	IF IsNull(li_comuna)THEN
		MessageBox("Atención","Debe Seleccionar una Comuna Previamente",Exclamation!)
		RETURN
	END IF
END IF


istr_info.titulo	= 'INFORME - PROVINCIAS COMUNAS'

OpenWithParm(vinf,istr_info)

  vinf.dw_1.DataObject = "dw_info_provcomunas"

vinf.dw_1.SetTransObject(sqlca)
li_fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),li_provincias,li_ciudad,li_comuna)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_numero.text = '" + '4' + "'")
	
	vinf.dw_1.Object.DataWindow.Zoom = 98
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_provincias_comunas_
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2002
integer y = 508
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_zona from statictext within w_info_provincias_comunas_
integer x = 119
integer y = 264
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Región"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_provincias_comunas_
integer x = 119
integer y = 388
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Provincia"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_provincias_comunas_
integer x = 119
integer y = 640
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Comuna"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_provincias_comunas_
integer x = 1385
integer y = 172
integer width = 197
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_provincias_comunas_
integer x = 119
integer y = 516
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Ciudad"
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_provincias_comunas_
integer x = 37
integer y = 136
integer width = 1833
integer height = 672
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_provincias_comunas_
integer x = 37
integer y = 28
integer width = 1833
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Provincias / Comunas"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type uo_muestraprovincias from uo_seleccion_provincias_mod within w_info_provincias_comunas_
integer x = 526
integer y = 352
integer width = 1056
integer taborder = 200
boolean bringtotop = true
end type

on uo_muestraprovincias.destroy
call uo_seleccion_provincias_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE 0
		uo_muestraciudades.Todos(True)
		uo_muestraciudades.cbx_Todos.Enabled	=	False
	CASE ELSE
		uo_muestraciudades.Filtra(Integer(istr_mant.argumento[1]),This.Codigo)
		uo_muestraciudades.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type uo_muestracomunas from uo_seleccion_comunasexp_mod within w_info_provincias_comunas_
integer x = 521
integer y = 600
integer width = 1047
integer taborder = 210
boolean bringtotop = true
end type

on uo_muestracomunas.destroy
call uo_seleccion_comunasexp_mod::destroy
end on

type ddlb_regiones from dropdownlistbox within w_info_provincias_comunas_
integer x = 530
integer y = 244
integer width = 864
integer height = 400
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
boolean vscrollbar = true
string item[] = {"I. Región","II. Región","III. Región","IV. Región","V. Región","VI. Región","VII. Región","VIII. Región","IX. Región","X. Región","XI. Región","XII. Región","XIII. R. Metropolitana"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	istr_Mant.Argumento[1] = '1'
ELSEIF Integer(index) = 2 THEN
	istr_Mant.Argumento[1] = '2'
ELSEIF Integer(index) = 3 THEN
	istr_Mant.Argumento[1] = '3'
ELSEIF Integer(index) = 4 THEN
	istr_Mant.Argumento[1] = '4'
ELSEIF Integer(index) = 5 THEN
	istr_Mant.Argumento[1] = '5'
ELSEIF Integer(index) = 6 THEN
	istr_Mant.Argumento[1] = '6'
ELSEIF Integer(index) = 7 THEN
	istr_Mant.Argumento[1] = '7'
ELSEIF Integer(index) = 8 THEN
	istr_Mant.Argumento[1] = '8'
ELSEIF Integer(index) = 9 THEN
	istr_Mant.Argumento[1] = '9'
ELSEIF Integer(index) = 10 THEN
	istr_Mant.Argumento[1] = '10'
ELSEIF Integer(index) = 11 THEN
	istr_Mant.Argumento[1] = '11'
ELSEIF Integer(index) = 12 THEN
	istr_Mant.Argumento[1] = '12'
ELSEIF Integer(index) = 13 THEN
	istr_Mant.Argumento[1] = '13'	
ELSEIF cbx_todos.checked THEN
	istr_mant.argumento[1] = '0'
END IF


end event

event modified;IF IsNull(This) THEN RETURN
	uo_muestraprovincias.Filtra(Integer(istr_mant.argumento[1]))
	uo_muestraprovincias.cbx_Todos.Enabled	=	True
		

end event

type uo_muestraciudades from uo_seleccion_ciudades_mod within w_info_provincias_comunas_
integer x = 521
integer y = 484
integer width = 1029
integer taborder = 210
boolean bringtotop = true
end type

on uo_muestraciudades.destroy
call uo_seleccion_ciudades_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE 0
		uo_muestracomunas.Todos(True)
		uo_muestracomunas.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestracomunas.Filtra(This.Codigo, Integer(istr_mant.argumento[1]),&
		                         uo_muestraprovincias.codigo)
		uo_muestracomunas.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type cbx_todos from checkbox within w_info_provincias_comunas_
integer x = 1440
integer y = 244
integer width = 133
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean checked = true
boolean righttoleft = true
end type

event clicked;IF This.Checked THEN
	ddlb_regiones.Enabled = False
	ddlb_regiones.SelectItem(0)
ELSE
	istr_mant.argumento[1] = ""
	ddlb_regiones.Enabled = True
END IF
end event

