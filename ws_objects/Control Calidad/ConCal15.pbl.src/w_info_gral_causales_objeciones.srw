$PBExportHeader$w_info_gral_causales_objeciones.srw
$PBExportComments$Ventana de Informe General de Causales Objeción
forward
global type w_info_gral_causales_objeciones from w_para_informes
end type
type gb_3 from groupbox within w_info_gral_causales_objeciones
end type
type st_1 from statictext within w_info_gral_causales_objeciones
end type
type em_fechadesde from editmask within w_info_gral_causales_objeciones
end type
type st_13 from statictext within w_info_gral_causales_objeciones
end type
type em_fechahasta from editmask within w_info_gral_causales_objeciones
end type
type st_zona from statictext within w_info_gral_causales_objeciones
end type
type st_33 from statictext within w_info_gral_causales_objeciones
end type
type cbx_todosfecha from checkbox within w_info_gral_causales_objeciones
end type
type rb_lotes from radiobutton within w_info_gral_causales_objeciones
end type
type gb_4 from groupbox within w_info_gral_causales_objeciones
end type
type st_2 from statictext within w_info_gral_causales_objeciones
end type
type st_4 from statictext within w_info_gral_causales_objeciones
end type
type st_5 from statictext within w_info_gral_causales_objeciones
end type
type rb_instancia from radiobutton within w_info_gral_causales_objeciones
end type
type gb_6 from groupbox within w_info_gral_causales_objeciones
end type
type gb_8 from groupbox within w_info_gral_causales_objeciones
end type
type st_3 from statictext within w_info_gral_causales_objeciones
end type
type dw_1 from uo_dw within w_info_gral_causales_objeciones
end type
type uo_selespecie from uo_seleccion_especie within w_info_gral_causales_objeciones
end type
type uo_selzona from uo_seleccion_zonas_mod within w_info_gral_causales_objeciones
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_gral_causales_objeciones
end type
type st_6 from statictext within w_info_gral_causales_objeciones
end type
type uo_selplanta from uo_seleccion_planta_mod within w_info_gral_causales_objeciones
end type
type uo_selpacking from uo_seleccion_plantapacking_mod within w_info_gral_causales_objeciones
end type
type st_7 from statictext within w_info_gral_causales_objeciones
end type
type st_12 from statictext within w_info_gral_causales_objeciones
end type
end forward

global type w_info_gral_causales_objeciones from w_para_informes
integer x = 14
integer y = 32
integer width = 3104
integer height = 1996
string title = "Causales de Objeción en Producto Terminado"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_3 gb_3
st_1 st_1
em_fechadesde em_fechadesde
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
rb_lotes rb_lotes
gb_4 gb_4
st_2 st_2
st_4 st_4
st_5 st_5
rb_instancia rb_instancia
gb_6 gb_6
gb_8 gb_8
st_3 st_3
dw_1 dw_1
uo_selespecie uo_selespecie
uo_selzona uo_selzona
uo_selproductor uo_selproductor
st_6 st_6
uo_selplanta uo_selplanta
uo_selpacking uo_selpacking
st_7 st_7
st_12 st_12
end type
global w_info_gral_causales_objeciones w_info_gral_causales_objeciones

type variables
str_mant istr_mant
Integer	ii_tipoinforme=1

w_info_resumen_inspeccion iw_planta


end variables

forward prototypes
public function boolean noexisteproductor (integer productor, integer tipo)
public function string sacaplanta (integer ii_codplanta)
end prototypes

public function boolean noexisteproductor (integer productor, integer tipo);Integer	li_Contador, li_zona
String	ls_Nombre


SELECT	prod_nombre
	INTO 	:ls_Nombre
	FROM 	dbo.productores
	WHERE prod_codigo = :productor;


IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productores")
	RETURN TRUE
ELSEIF ls_Nombre = "" THEN
	IF Tipo = 1 THEN
		messagebox("Atención","Código del Productor No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	ELSE		
		messagebox("Atención","Código Productor No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	END IF					
	RETURN TRUE
ELSE
	istr_Mant.Argumento[1]	=	ls_Nombre
	RETURN FALSE	
END IF
	
end function

public function string sacaplanta (integer ii_codplanta);String ls_planta

Select		IsNull(plde_nombre, ' ')
	Into		:ls_planta
	From		dbo.plantadesp
	Where	plde_codigo = :ii_codplanta
	Using 		Sqlca;

If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura tabla plantadesp  ")
	Return ' '
Else
	Return ls_planta
End If
end function

on w_info_gral_causales_objeciones.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_1=create st_1
this.em_fechadesde=create em_fechadesde
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.rb_lotes=create rb_lotes
this.gb_4=create gb_4
this.st_2=create st_2
this.st_4=create st_4
this.st_5=create st_5
this.rb_instancia=create rb_instancia
this.gb_6=create gb_6
this.gb_8=create gb_8
this.st_3=create st_3
this.dw_1=create dw_1
this.uo_selespecie=create uo_selespecie
this.uo_selzona=create uo_selzona
this.uo_selproductor=create uo_selproductor
this.st_6=create st_6
this.uo_selplanta=create uo_selplanta
this.uo_selpacking=create uo_selpacking
this.st_7=create st_7
this.st_12=create st_12
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_fechadesde
this.Control[iCurrent+4]=this.st_13
this.Control[iCurrent+5]=this.em_fechahasta
this.Control[iCurrent+6]=this.st_zona
this.Control[iCurrent+7]=this.st_33
this.Control[iCurrent+8]=this.cbx_todosfecha
this.Control[iCurrent+9]=this.rb_lotes
this.Control[iCurrent+10]=this.gb_4
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.st_5
this.Control[iCurrent+14]=this.rb_instancia
this.Control[iCurrent+15]=this.gb_6
this.Control[iCurrent+16]=this.gb_8
this.Control[iCurrent+17]=this.st_3
this.Control[iCurrent+18]=this.dw_1
this.Control[iCurrent+19]=this.uo_selespecie
this.Control[iCurrent+20]=this.uo_selzona
this.Control[iCurrent+21]=this.uo_selproductor
this.Control[iCurrent+22]=this.st_6
this.Control[iCurrent+23]=this.uo_selplanta
this.Control[iCurrent+24]=this.uo_selpacking
this.Control[iCurrent+25]=this.st_7
this.Control[iCurrent+26]=this.st_12
end on

on w_info_gral_causales_objeciones.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_1)
destroy(this.em_fechadesde)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.rb_lotes)
destroy(this.gb_4)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.rb_instancia)
destroy(this.gb_6)
destroy(this.gb_8)
destroy(this.st_3)
destroy(this.dw_1)
destroy(this.uo_selespecie)
destroy(this.uo_selzona)
destroy(this.uo_selproductor)
destroy(this.st_6)
destroy(this.uo_selplanta)
destroy(this.uo_selpacking)
destroy(this.st_7)
destroy(this.st_12)
end on

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelZona.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_Selpacking.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelEspecie.Seleccion(False, False)
	uo_SelZona.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelPlanta.Seleccion(True, True)
	uo_Selpacking.Seleccion(True, True)

	uo_SelProductor.cbx_Consolida.Checked=True
	uo_SelProductor.cbx_Consolida.TriggerEvent('clicked')
	uo_SelPlanta.cbx_Todos.Checked=True
	uo_SelPlanta.cbx_Consolida.Checked=False
	uo_SelEspecie.iuo_Codigo.Existe(11, False, Sqlca)
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1] = 11
	uo_SelEspecie.Codigo	= uo_SelEspecie.iuo_Codigo.Codigo
	uo_SelEspecie.Nombre	= uo_SelEspecie.iuo_Codigo.Nombre
	uo_Selpacking.cbx_Todos.Checked=False
	uo_Selpacking.cbx_Consolida.Checked= True
	
	em_fechadesde.text	=	String(Today())
	em_fechahasta.text	=	String(Today())
	ii_tipoinforme			=	2	
End If
end event

event resize;call super::resize;pb_Excel.x			=	pb_acepta.x
pb_Excel.y			=	pb_acepta.y + 195
pb_Excel.width		=	235
pb_Excel.height	=	195

pb_salir.y			=	pb_Excel.y + 195

end event

type pb_excel from w_para_informes`pb_excel within w_info_gral_causales_objeciones
integer x = 2491
integer y = 360
end type

type st_computador from w_para_informes`st_computador within w_info_gral_causales_objeciones
end type

type st_usuario from w_para_informes`st_usuario within w_info_gral_causales_objeciones
end type

type st_temporada from w_para_informes`st_temporada within w_info_gral_causales_objeciones
end type

type p_logo from w_para_informes`p_logo within w_info_gral_causales_objeciones
end type

type st_titulo from w_para_informes`st_titulo within w_info_gral_causales_objeciones
integer width = 2085
string text = "Informe Causales de Objeción en Producto Terminado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_gral_causales_objeciones
string tag = "Imprimir Reporte"
integer x = 2496
integer y = 656
integer taborder = 110
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_Fila
Date		ld_FechaEmbaini, ld_FechaEmbafin

SetPointer(HourGlass!)

If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

istr_info.titulo	= 'INFORME DE CAUSALES DE OBJECION EN FRUTA REVISADA' 
OpenWithParm(vinf,istr_info)

If uo_SelEspecie.codigo = 11 Then//Uvas
	vinf.dw_1.DataObject = "dw_info_causales_objec_fruta_rev_2"
Else	
	vinf.dw_1.DataObject = "dw_info_causales_objec_fruta_rev3"
End If

vinf.dw_1.SetTransObject(sqlca)

If uo_SelEspecie.codigo = 11 Then//Uvas
	li_fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelZona.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, uo_SelEspecie.Codigo, uo_SelProductor.Codigo, ii_tipoinforme )
Else	
	li_fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelZona.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, uo_SelEspecie.Codigo, uo_SelProductor.Codigo, ii_tipoinforme, uo_selplanta.Codigo, uo_selpacking.Codigo )
End If

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.",  StopSign!, Ok!)
Else	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 74
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_gral_causales_objeciones
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2496
integer y = 936
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_3 from groupbox within w_info_gral_causales_objeciones
integer x = 379
integer y = 1416
integer width = 1815
integer height = 244
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = " Fecha "
end type

type st_1 from statictext within w_info_gral_causales_objeciones
integer x = 443
integer y = 1056
integer width = 402
integer height = 76
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

type em_fechadesde from editmask within w_info_gral_causales_objeciones
integer x = 878
integer y = 1528
integer width = 494
integer height = 92
integer taborder = 90
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

type st_13 from statictext within w_info_gral_causales_objeciones
integer x = 1435
integer y = 1456
integer width = 407
integer height = 52
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
alignment alignment = center!
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_gral_causales_objeciones
integer x = 1399
integer y = 1528
integer width = 489
integer height = 92
integer taborder = 100
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

type st_zona from statictext within w_info_gral_causales_objeciones
integer x = 443
integer y = 928
integer width = 402
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

type st_33 from statictext within w_info_gral_causales_objeciones
integer x = 443
integer y = 808
integer width = 402
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

type cbx_todosfecha from checkbox within w_info_gral_causales_objeciones
integer x = 1925
integer y = 1540
integer width = 87
integer height = 64
integer taborder = 80
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
	em_fechadesde.Enabled		=	False
	em_fechahasta.Enabled	=	False
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0
end event

type rb_lotes from radiobutton within w_info_gral_causales_objeciones
integer x = 503
integer y = 596
integer width = 942
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
string text = "Lotes Aprobados Comercial"
end type

event clicked;ii_tipoinforme = 1

end event

type gb_4 from groupbox within w_info_gral_causales_objeciones
integer x = 379
integer y = 696
integer width = 1815
integer height = 712
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_2 from statictext within w_info_gral_causales_objeciones
integer x = 448
integer y = 1532
integer width = 402
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_gral_causales_objeciones
integer x = 1728
integer y = 736
integer width = 187
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
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_gral_causales_objeciones
integer x = 1938
integer y = 736
integer width = 187
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
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

type rb_instancia from radiobutton within w_info_gral_causales_objeciones
integer x = 1527
integer y = 596
integer width = 599
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lotes Objetados"
boolean checked = true
end type

event clicked;ii_tipoinforme = 2




end event

type gb_6 from groupbox within w_info_gral_causales_objeciones
integer x = 379
integer y = 500
integer width = 1815
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_8 from groupbox within w_info_gral_causales_objeciones
integer x = 329
integer y = 448
integer width = 1915
integer height = 1260
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_3 from statictext within w_info_gral_causales_objeciones
integer x = 247
integer y = 408
integer width = 2085
integer height = 1348
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

event clicked;SetPointer(Arrow!)
Integer	li_Fila
Date		ld_FechaEmbaini, ld_FechaEmbafin
String		ls_Ruta, ls_Archivo, ls_planta

SetPointer(HourGlass!)
RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

If uo_SelEspecie.codigo = 11 Then
	dw_1.DataObject = "dw_info_causalesobjecion_uva_excel"
Else
	dw_1.DataObject = "dw_info_causalesobjecion_cerezas_excel"
End If

dw_1.SetTransObject(sqlca)
ls_planta = sacaplanta(gi_codplanta)
ls_Archivo = '\CO_' + uo_SelEspecie.Nombre +'_'+ trim(ls_planta)+ '.xls'

li_fila = dw_1.Retrieve(gi_CodExport, uo_SelZona.Codigo, uo_SelPlanta.Codigo, ld_FechaEmbaini, ld_FechaEmbafin,uo_SelEspecie.Codigo, uo_SelProductor.Codigo, ii_tipoinforme,uo_selpacking.Codigo) 

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este Archivo.", StopSign!, Ok!)
Else	
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, Excel8!, True) = 1 Then
		MessageBox('Atencion', 'Se genero archivo ' + ls_Archivo)
	Else
		MessageBox('Atencion', 'No se pudo generar archivo ' + ls_Archivo)		
	End If	
End If

SetPointer(Arrow!)
end event

type dw_1 from uo_dw within w_info_gral_causales_objeciones
boolean visible = false
integer x = 2519
integer y = 1232
integer width = 105
integer height = 108
integer taborder = 11
boolean bringtotop = true
boolean vscrollbar = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_gral_causales_objeciones
event destroy ( )
integer x = 859
integer y = 808
integer width = 864
integer height = 84
integer taborder = 130
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selzona from uo_seleccion_zonas_mod within w_info_gral_causales_objeciones
event destroy ( )
integer x = 864
integer y = 904
integer taborder = 40
boolean bringtotop = true
end type

on uo_selzona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return


Choose Case This.Codigo
	Case -1
		uo_SelProductor.Todos(True)
	Case -9
		uo_SelProductor.Filtra(-1)
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
	
End Choose 
end event

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_gral_causales_objeciones
event destroy ( )
integer x = 864
integer y = 1020
integer taborder = 70
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type st_6 from statictext within w_info_gral_causales_objeciones
integer x = 443
integer y = 1172
integer width = 402
integer height = 76
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

type uo_selplanta from uo_seleccion_planta_mod within w_info_gral_causales_objeciones
integer x = 864
integer y = 1140
integer height = 128
integer taborder = 80
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_planta_mod::destroy
end on

type uo_selpacking from uo_seleccion_plantapacking_mod within w_info_gral_causales_objeciones
integer x = 864
integer y = 1260
integer height = 128
integer taborder = 21
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_plantapacking_mod::destroy
end on

type st_7 from statictext within w_info_gral_causales_objeciones
integer x = 443
integer y = 1288
integer width = 402
integer height = 76
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
string text = "Packing"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_gral_causales_objeciones
integer x = 905
integer y = 1456
integer width = 407
integer height = 52
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
alignment alignment = center!
boolean focusrectangle = false
end type

