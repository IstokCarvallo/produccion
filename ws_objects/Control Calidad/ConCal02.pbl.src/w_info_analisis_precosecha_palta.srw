$PBExportHeader$w_info_analisis_precosecha_palta.srw
forward
global type w_info_analisis_precosecha_palta from w_para_informes
end type
type gb_6 from groupbox within w_info_analisis_precosecha_palta
end type
type st_33 from statictext within w_info_analisis_precosecha_palta
end type
type st_zona from statictext within w_info_analisis_precosecha_palta
end type
type st_variedad from statictext within w_info_analisis_precosecha_palta
end type
type em_fechaemb from editmask within w_info_analisis_precosecha_palta
end type
type em_fechahasta from editmask within w_info_analisis_precosecha_palta
end type
type st_desdeins from statictext within w_info_analisis_precosecha_palta
end type
type st_hastains from statictext within w_info_analisis_precosecha_palta
end type
type cbx_fechaemba from checkbox within w_info_analisis_precosecha_palta
end type
type st_2 from statictext within w_info_analisis_precosecha_palta
end type
type st_4 from statictext within w_info_analisis_precosecha_palta
end type
type st_6 from statictext within w_info_analisis_precosecha_palta
end type
type st_44 from statictext within w_info_analisis_precosecha_palta
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_analisis_precosecha_palta
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_analisis_precosecha_palta
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_analisis_precosecha_palta
end type
type st_1 from statictext within w_info_analisis_precosecha_palta
end type
type uo_selpredio from uo_seleccion_prodpredio_mod within w_info_analisis_precosecha_palta
end type
type uo_selcuarteles from uo_seleccion_prodcuarteles_mod within w_info_analisis_precosecha_palta
end type
type st_3 from statictext within w_info_analisis_precosecha_palta
end type
end forward

global type w_info_analisis_precosecha_palta from w_para_informes
integer x = 14
integer y = 32
integer width = 2606
integer height = 1980
string title = "Detalle Precosecha Paltas"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_6 gb_6
st_33 st_33
st_zona st_zona
st_variedad st_variedad
em_fechaemb em_fechaemb
em_fechahasta em_fechahasta
st_desdeins st_desdeins
st_hastains st_hastains
cbx_fechaemba cbx_fechaemba
st_2 st_2
st_4 st_4
st_6 st_6
st_44 st_44
uo_selzonas uo_selzonas
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
st_1 st_1
uo_selpredio uo_selpredio
uo_selcuarteles uo_selcuarteles
st_3 st_3
end type
global w_info_analisis_precosecha_palta w_info_analisis_precosecha_palta

type variables
str_busqueda	istr_busq
str_mant 		istr_mant

uo_ctlcaldanoespecie iuo_ctlcaldanoespecie
end variables

forward prototypes
public function boolean noexisteembalaje (string embalaje)
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexistevariecalibre (integer variedad, string calibre)
public function boolean noexistevariedad (integer variedad)
public function boolean noexisteinspector (integer codigo)
public function boolean noexisteinspeccion (integer codigo)
public function boolean noexisteproductor (long ai_zona, integer al_productor)
end prototypes

public function boolean noexisteembalaje (string embalaje);//String ls_Nombre,ls_Codigo
//
//ls_codigo	=	''
//
//Select emba_codigo,emba_nombre
//Into :ls_Codigo,:ls_Nombre
//From dba.embalajes
//Where emba_codigo = :embalaje;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajes")
//	RETURN TRUE
//ELSEIF ls_Codigo = '' THEN
//	MessageBox("Atencion","Código de Embalaje no Existe, Ingrese Otro Código",Exclamation!)
//	RETURN TRUE
//ELSE	
//	em_descripcion.text	=	ls_Nombre
	RETURN FALSE	
//END IF
	

end function

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_cliente,li_zona

li_cliente	=	gi_CodExport

Select Count(*)
Into :li_Contador
From dbo.plantadesp
Where plde_codigo = :planta
and	plde_tipopl	= :Tipo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	IF Tipo = 1 THEN
		messagebox("Atención","Código Planta No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	ELSE		
		messagebox("Atención","Código Packing No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	END IF					
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

public function boolean noexistevariecalibre (integer variedad, string calibre);String ls_Codigo
Integer li_variedad, li_especie

li_especie  =	uo_selvariedad.especie
li_variedad	= 	uo_selvariedad.codigo

Calibre		= 	Calibre + Fill(" ",3 - Len(Calibre))
ls_codigo	=	''

IF li_variedad <> 0 THEN

	Select vaca_calibr
	Into :ls_Codigo
	From dbo.variecalibre
	Where espe_codigo =  :li_especie
	And   vari_codigo =  :li_variedad
	And   vaca_calibr	=	:Calibre;
		
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
		RETURN TRUE
	ELSEIF ls_Codigo = '' THEN
		MessageBox("Atencion","Código de Calibre no Existe, Ingrese Otro Código",Exclamation!)
		RETURN TRUE
	ELSE	
		RETURN FALSE	
	END IF
ELSE
	MessageBox("Atención","Previamente Debe Elegir Una Variedad",Exclamation!)
		Return True
END IF		
		
end function

public function boolean noexistevariedad (integer variedad);Integer li_Contador

Select Count(*)
Into :li_Contador
From dbo.variedades
Where vari_codigo = :variedad;

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

public function boolean noexisteinspector (integer codigo);Integer li_Contador

Select Count(*)
Into :li_Contador
From dbo.ctlcalinspectores
Where ccin_codigo = :codigo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Ctlcalinspectores")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Inspector No Existe" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexisteinspeccion (integer codigo);Integer li_Contador

Select Count(*)
Into :li_Contador
From dbo.ctlcaltiposinspeccion
Where ccti_codigo = :codigo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla ctlcaltiposinspeccion")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Inspección No Existe" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF


end function

public function boolean noexisteproductor (long ai_zona, integer al_productor);Integer li_Contador

Select Count(*)
Into :li_Contador
From dbo.productores
Where prod_codigo = :al_productor and
		zona_codigo = :ai_zona ;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Productor No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

on w_info_analisis_precosecha_palta.create
int iCurrent
call super::create
this.gb_6=create gb_6
this.st_33=create st_33
this.st_zona=create st_zona
this.st_variedad=create st_variedad
this.em_fechaemb=create em_fechaemb
this.em_fechahasta=create em_fechahasta
this.st_desdeins=create st_desdeins
this.st_hastains=create st_hastains
this.cbx_fechaemba=create cbx_fechaemba
this.st_2=create st_2
this.st_4=create st_4
this.st_6=create st_6
this.st_44=create st_44
this.uo_selzonas=create uo_selzonas
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
this.st_1=create st_1
this.uo_selpredio=create uo_selpredio
this.uo_selcuarteles=create uo_selcuarteles
this.st_3=create st_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_6
this.Control[iCurrent+2]=this.st_33
this.Control[iCurrent+3]=this.st_zona
this.Control[iCurrent+4]=this.st_variedad
this.Control[iCurrent+5]=this.em_fechaemb
this.Control[iCurrent+6]=this.em_fechahasta
this.Control[iCurrent+7]=this.st_desdeins
this.Control[iCurrent+8]=this.st_hastains
this.Control[iCurrent+9]=this.cbx_fechaemba
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.st_44
this.Control[iCurrent+14]=this.uo_selzonas
this.Control[iCurrent+15]=this.uo_selvariedad
this.Control[iCurrent+16]=this.uo_selproductor
this.Control[iCurrent+17]=this.st_1
this.Control[iCurrent+18]=this.uo_selpredio
this.Control[iCurrent+19]=this.uo_selcuarteles
this.Control[iCurrent+20]=this.st_3
end on

on w_info_analisis_precosecha_palta.destroy
call super::destroy
destroy(this.gb_6)
destroy(this.st_33)
destroy(this.st_zona)
destroy(this.st_variedad)
destroy(this.em_fechaemb)
destroy(this.em_fechahasta)
destroy(this.st_desdeins)
destroy(this.st_hastains)
destroy(this.cbx_fechaemba)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.st_44)
destroy(this.uo_selzonas)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
destroy(this.st_1)
destroy(this.uo_selpredio)
destroy(this.uo_selcuarteles)
destroy(this.st_3)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelZonas.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPredio.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelCuarteles.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelZonas.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelPredio.Seleccion(True, False)
	uo_SelCuarteles.Seleccion(True, False)
	uo_selVariedad.Filtra(81)
	uo_SelPredio.cbx_Todos.Checked = True
	uo_SelCuarteles.cbx_Todos.Checked = True
	uo_SelCuarteles.Codigo = -1
	uo_SelPredio.Codigo = -1
	
	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	160
	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	150
	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	210
	uo_SelPredio.Ancho(210)
	uo_SelCuarteles.Ancho(150)

	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	uo_SelPredio.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
	
				
	em_fechahasta.Text	=	String(Today())
	em_fechaemb.Text	=	String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_analisis_precosecha_palta
end type

type st_computador from w_para_informes`st_computador within w_info_analisis_precosecha_palta
integer x = 1618
integer y = 148
end type

type st_usuario from w_para_informes`st_usuario within w_info_analisis_precosecha_palta
integer x = 1618
end type

type st_temporada from w_para_informes`st_temporada within w_info_analisis_precosecha_palta
integer x = 1618
integer y = 4
end type

type p_logo from w_para_informes`p_logo within w_info_analisis_precosecha_palta
end type

type st_titulo from w_para_informes`st_titulo within w_info_analisis_precosecha_palta
integer width = 1664
string text = "Informe Analisis Precosecha"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_analisis_precosecha_palta
string tag = "Imprimir Reporte"
integer x = 2144
integer y = 532
integer taborder = 370
end type

event pb_acepta::clicked;Integer	li_fila
Date		ld_Desde,ld_Hasta

If cbx_fechaemba.Checked Then
	ld_Desde=	Date(01/01/2000)
	ld_Hasta =	Today()
Else
	ld_Desde = Date(em_fechaemb.Text)
	ld_Hasta = Date(em_fechahasta.Text)
End If

istr_info.titulo	= 'ANALISIS PRECOSECHA DE PALTAS'
OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_analisisprecosecha_paltas"	
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(81, uo_SelZonas.Codigo, uo_SelProductor.Codigo, uo_SelPredio.Codigo, uo_SelCuarteles.Codigo, &
							uo_SelVariedad.Codigo, ld_Desde, ld_Hasta)

If li_fila = -1 Then
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.",  StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_analisis_precosecha_palta
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2144
integer y = 844
integer taborder = 380
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_6 from groupbox within w_info_analisis_precosecha_palta
integer x = 288
integer y = 1376
integer width = 1563
integer height = 292
integer taborder = 120
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Analisis "
end type

type st_33 from statictext within w_info_analisis_precosecha_palta
integer x = 352
integer y = 708
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -8
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

type st_zona from statictext within w_info_analisis_precosecha_palta
integer x = 352
integer y = 560
integer width = 411
integer height = 84
boolean bringtotop = true
integer textsize = -8
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

type st_variedad from statictext within w_info_analisis_precosecha_palta
integer x = 352
integer y = 1160
integer width = 279
integer height = 84
boolean bringtotop = true
integer textsize = -8
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

type em_fechaemb from editmask within w_info_analisis_precosecha_palta
integer x = 503
integer y = 1524
integer width = 411
integer height = 80
integer taborder = 280
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_fechahasta from editmask within w_info_analisis_precosecha_palta
integer x = 933
integer y = 1524
integer width = 411
integer height = 80
integer taborder = 290
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_desdeins from statictext within w_info_analisis_precosecha_palta
integer x = 517
integer y = 1464
integer width = 375
integer height = 56
boolean bringtotop = true
integer textsize = -8
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

type st_hastains from statictext within w_info_analisis_precosecha_palta
integer x = 942
integer y = 1464
integer width = 398
integer height = 56
boolean bringtotop = true
integer textsize = -8
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

type cbx_fechaemba from checkbox within w_info_analisis_precosecha_palta
integer x = 1408
integer y = 1532
integer width = 82
integer height = 64
integer taborder = 300
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_fechaemb.enabled = False
	em_fechahasta.enabled = False
Else
	em_fechaemb.enabled = True
	em_fechahasta.enabled = True
End If
end event

type st_2 from statictext within w_info_analisis_precosecha_palta
integer x = 1682
integer y = 452
integer width = 192
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type st_4 from statictext within w_info_analisis_precosecha_palta
integer x = 251
integer y = 1356
integer width = 1664
integer height = 360
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_analisis_precosecha_palta
integer x = 1367
integer y = 1468
integer width = 169
integer height = 52
boolean bringtotop = true
integer textsize = -8
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

type st_44 from statictext within w_info_analisis_precosecha_palta
integer x = 251
integer y = 440
integer width = 1664
integer height = 916
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

type uo_selzonas from uo_seleccion_zonas_mod within w_info_analisis_precosecha_palta
event destroy ( )
integer x = 791
integer y = 516
integer width = 1042
integer taborder = 10
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1)
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_analisis_precosecha_palta
event destroy ( )
integer x = 791
integer y = 1116
integer width = 1042
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_analisis_precosecha_palta
event destroy ( )
integer x = 791
integer y = 664
integer width = 1042
integer taborder = 390
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPredio.Filtra(-1)
		uo_SelCuarteles.Filtra(-1, uo_SelPredio.Codigo)
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)
		uo_SelCuarteles.Filtra(This.Codigo, uo_SelPredio.Codigo)
		
End Choose
end event

type st_1 from statictext within w_info_analisis_precosecha_palta
integer x = 352
integer y = 1008
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cuartel"
boolean focusrectangle = false
end type

type uo_selpredio from uo_seleccion_prodpredio_mod within w_info_analisis_precosecha_palta
integer x = 786
integer y = 812
integer width = 1042
integer taborder = 400
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_prodpredio_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCuarteles.Filtra(uo_SelProductor.Codigo, -1)
		
	Case Else
		uo_SelCuarteles.Filtra(uo_SelProductor.Codigo, This.Codigo)
		
End Choose
end event

event constructor;call super::constructor;dw_Seleccion.Object.Codigo.Dddw.Name				=	'dw_mues_prodpredio_sel'
end event

type uo_selcuarteles from uo_seleccion_prodcuarteles_mod within w_info_analisis_precosecha_palta
integer x = 791
integer y = 964
integer width = 1042
integer taborder = 390
boolean bringtotop = true
end type

on uo_selcuarteles.destroy
call uo_seleccion_prodcuarteles_mod::destroy
end on

type st_3 from statictext within w_info_analisis_precosecha_palta
integer x = 352
integer y = 856
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Huerto"
boolean focusrectangle = false
end type

