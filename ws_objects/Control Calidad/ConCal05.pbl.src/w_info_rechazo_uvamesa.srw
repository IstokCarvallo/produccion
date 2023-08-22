$PBExportHeader$w_info_rechazo_uvamesa.srw
$PBExportComments$Ventana de Informe de Resumen Objeciones
forward
global type w_info_rechazo_uvamesa from w_para_informes
end type
type st_1 from statictext within w_info_rechazo_uvamesa
end type
type em_desde from editmask within w_info_rechazo_uvamesa
end type
type st_12 from statictext within w_info_rechazo_uvamesa
end type
type st_13 from statictext within w_info_rechazo_uvamesa
end type
type em_hasta from editmask within w_info_rechazo_uvamesa
end type
type st_zona from statictext within w_info_rechazo_uvamesa
end type
type st_3 from statictext within w_info_rechazo_uvamesa
end type
type cbx_cons from checkbox within w_info_rechazo_uvamesa
end type
type st_8 from statictext within w_info_rechazo_uvamesa
end type
type st_11 from statictext within w_info_rechazo_uvamesa
end type
type st_9 from statictext within w_info_rechazo_uvamesa
end type
type st_10 from statictext within w_info_rechazo_uvamesa
end type
type st_4 from statictext within w_info_rechazo_uvamesa
end type
type st_44 from statictext within w_info_rechazo_uvamesa
end type
type st_2 from statictext within w_info_rechazo_uvamesa
end type
type cbx_todo from checkbox within w_info_rechazo_uvamesa
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_rechazo_uvamesa
end type
type uo_selespecie from uo_seleccion_especie within w_info_rechazo_uvamesa
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_rechazo_uvamesa
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_rechazo_uvamesa
end type
type uo_selfrigo from uo_seleccion_frigorifico_mod within w_info_rechazo_uvamesa
end type
end forward

global type w_info_rechazo_uvamesa from w_para_informes
integer x = 14
integer y = 32
integer width = 2734
integer height = 1872
string title = "Informe de Rechazos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
em_desde em_desde
st_12 st_12
st_13 st_13
em_hasta em_hasta
st_zona st_zona
st_3 st_3
cbx_cons cbx_cons
st_8 st_8
st_11 st_11
st_9 st_9
st_10 st_10
st_4 st_4
st_44 st_44
st_2 st_2
cbx_todo cbx_todo
uo_selzonas uo_selzonas
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
uo_selfrigo uo_selfrigo
end type
global w_info_rechazo_uvamesa w_info_rechazo_uvamesa

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, Codigo
String	is_report, Nombre

DataWindowChild		idwc_zona,idwc_planta, idwc_cliente,idwc_variedad,&
                     idwc_especie,idwc_productores

uo_zonas					iuo_zonas
uo_especie				iuo_especie
uo_productores       iuo_productores

end variables

forward prototypes
public function boolean noexistevariedad (integer variedad)
protected function boolean noexisteplanta (integer planta)
end prototypes

public function boolean noexistevariedad (integer variedad);Integer li_Contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.variedades
	WHERE	vari_codigo = :variedad;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Variedades")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Variedad No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

protected function boolean noexisteplanta (integer planta);Integer li_Contador, li_cliente,li_zona

li_cliente	=	gi_CodExport

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.plantadesp
	WHERE plde_codigo = :planta
	AND	plde_tipopl	= 1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
		messagebox("Atención","Código Frigorífico No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

on w_info_rechazo_uvamesa.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_desde=create em_desde
this.st_12=create st_12
this.st_13=create st_13
this.em_hasta=create em_hasta
this.st_zona=create st_zona
this.st_3=create st_3
this.cbx_cons=create cbx_cons
this.st_8=create st_8
this.st_11=create st_11
this.st_9=create st_9
this.st_10=create st_10
this.st_4=create st_4
this.st_44=create st_44
this.st_2=create st_2
this.cbx_todo=create cbx_todo
this.uo_selzonas=create uo_selzonas
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
this.uo_selfrigo=create uo_selfrigo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_desde
this.Control[iCurrent+3]=this.st_12
this.Control[iCurrent+4]=this.st_13
this.Control[iCurrent+5]=this.em_hasta
this.Control[iCurrent+6]=this.st_zona
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.cbx_cons
this.Control[iCurrent+9]=this.st_8
this.Control[iCurrent+10]=this.st_11
this.Control[iCurrent+11]=this.st_9
this.Control[iCurrent+12]=this.st_10
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.st_44
this.Control[iCurrent+15]=this.st_2
this.Control[iCurrent+16]=this.cbx_todo
this.Control[iCurrent+17]=this.uo_selzonas
this.Control[iCurrent+18]=this.uo_selespecie
this.Control[iCurrent+19]=this.uo_selvariedad
this.Control[iCurrent+20]=this.uo_selproductor
this.Control[iCurrent+21]=this.uo_selfrigo
end on

on w_info_rechazo_uvamesa.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_desde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_hasta)
destroy(this.st_zona)
destroy(this.st_3)
destroy(this.cbx_cons)
destroy(this.st_8)
destroy(this.st_11)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_4)
destroy(this.st_44)
destroy(this.st_2)
destroy(this.cbx_todo)
destroy(this.uo_selzonas)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
destroy(this.uo_selfrigo)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelEspecie.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelZonas.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelFrigo.Codigo)		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelProductor.Seleccion(True, True)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, True)
	uo_SelZonas.Seleccion(True, True)
	uo_SelFrigo.Seleccion(True, True)
	
	uo_SelEspecie.Inicia(11)
	uo_SelVariedad.Filtra(11)

	em_desde.text	  = '01/' + String(Today(), 'mm/yyyy')
	em_hasta.text	  = String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_rechazo_uvamesa
end type

type st_computador from w_para_informes`st_computador within w_info_rechazo_uvamesa
end type

type st_usuario from w_para_informes`st_usuario within w_info_rechazo_uvamesa
end type

type st_temporada from w_para_informes`st_temporada within w_info_rechazo_uvamesa
end type

type p_logo from w_para_informes`p_logo within w_info_rechazo_uvamesa
end type

type st_titulo from w_para_informes`st_titulo within w_info_rechazo_uvamesa
integer width = 1938
string text = "Informe de Rechazos "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_rechazo_uvamesa
string tag = "Imprimir Reporte"
integer x = 2350
integer y = 760
integer taborder = 310
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_ConsFecha
Date		ld_desde, ld_hasta

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME DE RECHAZOS'

If cbx_Cons.Checked Then
	li_ConsFecha	=	1
	ld_desde 		=	Date('19000101')
	ld_hasta 			=	Today()
ElseIf cbx_Todo.Checked Then
	li_ConsFecha	=	0
	ld_desde 		=	Date('19000101')
	ld_hasta 			=	Today()	
Else
	ld_desde 		=	Date(em_desde.Text)
	ld_hasta 			=	Date(em_hasta.Text)
	li_ConsFecha	=	0
End If

OpenWithParm(vinf,istr_info)
 
If uo_SelEspecie.Codigo = 11 Then
    vinf.dw_1.DataObject = "dw_info_rechazo_uvamesa"
Else
	vinf.dw_1.DataObject = "dw_info_rechazo_especies"
End If

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_codexport, uo_SelZonas.Codigo, uo_SelFrigo.Codigo, uo_SelProductor.Codigo, uo_SelEspecie.Codigo,&
					uo_SelVariedad.Codigo, ld_desde, ld_hasta, li_consfecha)

IF li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_rechazo_uvamesa
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2345
integer y = 1040
integer taborder = 320
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_rechazo_uvamesa
integer x = 416
integer y = 720
integer width = 379
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
string text = "Frigorífico"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_rechazo_uvamesa
integer x = 814
integer y = 1408
integer width = 466
integer height = 92
integer taborder = 290
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_rechazo_uvamesa
integer x = 814
integer y = 1344
integer width = 201
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
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_rechazo_uvamesa
integer x = 1294
integer y = 1344
integer width = 247
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_rechazo_uvamesa
integer x = 1294
integer y = 1412
integer width = 466
integer height = 92
integer taborder = 300
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_zona from statictext within w_info_rechazo_uvamesa
integer x = 416
integer y = 596
integer width = 379
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
string text = "Zona "
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_rechazo_uvamesa
integer x = 416
integer y = 976
integer width = 379
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

type cbx_cons from checkbox within w_info_rechazo_uvamesa
integer x = 1957
integer y = 1412
integer width = 119
integer height = 84
integer taborder = 270
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = " "
end type

event clicked;If This.Checked Then
	cbx_Todo.Checked		=	False
	em_Desde.Enabled	=	False
	em_Hasta.Enabled		=	False
Else
	cbx_Todo.Checked		=	False	
	em_Desde.Enabled	=	True
	em_Hasta.Enabled		=	True
	em_desde.text	  = '01/' + String(Today(), 'mm/yyyy')
	em_hasta.text	  = String(Today())
End If
end event

type st_8 from statictext within w_info_rechazo_uvamesa
integer x = 251
integer y = 1252
integer width = 1938
integer height = 360
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = roman!
string facename = "Sylfaen"
boolean underline = true
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_rechazo_uvamesa
integer x = 416
integer y = 848
integer width = 379
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_rechazo_uvamesa
integer x = 1687
integer y = 500
integer width = 187
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
string text = "Todos"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_rechazo_uvamesa
integer x = 1902
integer y = 500
integer width = 187
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
string text = "Cons."
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_rechazo_uvamesa
integer x = 411
integer y = 1420
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
string text = "Fecha Emb."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_rechazo_uvamesa
integer x = 251
integer y = 440
integer width = 1938
integer height = 812
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = roman!
string facename = "Sylfaen"
boolean underline = true
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_rechazo_uvamesa
integer x = 411
integer y = 1112
integer width = 379
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
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_todo from checkbox within w_info_rechazo_uvamesa
integer x = 1797
integer y = 1416
integer width = 119
integer height = 84
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
	em_Desde.Enabled	=	False
	em_Hasta.Enabled		=	False
	cbx_Cons.Checked		=	False
Else
	cbx_Cons.Checked		=	False	
	em_Desde.Enabled	=	True
	em_Hasta.Enabled		=	True
	em_desde.text	  = '01/' + String(Today(), 'mm/yyyy')
	em_hasta.text	  = String(Today())
End If
end event

type uo_selzonas from uo_seleccion_zonas_mod within w_info_rechazo_uvamesa
event destroy ( )
integer x = 805
integer y = 560
integer taborder = 30
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(0)
		uo_SelFrigo.Filtra(0)
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
		uo_SelFrigo.Filtra(This.Codigo)
		
End Choose
end event

type uo_selespecie from uo_seleccion_especie within w_info_rechazo_uvamesa
event destroy ( )
integer x = 814
integer y = 836
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_rechazo_uvamesa
event destroy ( )
integer x = 814
integer y = 940
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_rechazo_uvamesa
event destroy ( )
integer x = 814
integer y = 1072
integer height = 124
integer taborder = 320
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_selfrigo from uo_seleccion_frigorifico_mod within w_info_rechazo_uvamesa
event destroy ( )
integer x = 814
integer y = 692
integer taborder = 40
boolean bringtotop = true
end type

on uo_selfrigo.destroy
call uo_seleccion_frigorifico_mod::destroy
end on

