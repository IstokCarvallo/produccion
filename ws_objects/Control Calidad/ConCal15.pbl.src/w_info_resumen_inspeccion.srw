$PBExportHeader$w_info_resumen_inspeccion.srw
$PBExportComments$Ventana de Informe Revisión Inspección
forward
global type w_info_resumen_inspeccion from w_para_informes
end type
type st_1 from statictext within w_info_resumen_inspeccion
end type
type em_fechadesde from editmask within w_info_resumen_inspeccion
end type
type st_13 from statictext within w_info_resumen_inspeccion
end type
type em_fechahasta from editmask within w_info_resumen_inspeccion
end type
type st_zona from statictext within w_info_resumen_inspeccion
end type
type st_33 from statictext within w_info_resumen_inspeccion
end type
type cbx_todosfecha from checkbox within w_info_resumen_inspeccion
end type
type st_12 from statictext within w_info_resumen_inspeccion
end type
type st_3 from statictext within w_info_resumen_inspeccion
end type
type st_5 from statictext within w_info_resumen_inspeccion
end type
type st_6 from statictext within w_info_resumen_inspeccion
end type
type cbx_filtro from checkbox within w_info_resumen_inspeccion
end type
type ddlb_filtro from dropdownlistbox within w_info_resumen_inspeccion
end type
type st_8 from statictext within w_info_resumen_inspeccion
end type
type st_4 from statictext within w_info_resumen_inspeccion
end type
type st_2 from statictext within w_info_resumen_inspeccion
end type
type st_7 from statictext within w_info_resumen_inspeccion
end type
type gb_5 from groupbox within w_info_resumen_inspeccion
end type
type st_9 from statictext within w_info_resumen_inspeccion
end type
type gb_4 from groupbox within w_info_resumen_inspeccion
end type
type sle_1 from singlelineedit within w_info_resumen_inspeccion
end type
type uo_selzona from uo_seleccion_zonas_mod within w_info_resumen_inspeccion
end type
type uo_selvari from uo_seleccion_variedad_mod within w_info_resumen_inspeccion
end type
type uo_selprod from uo_seleccion_productor_mod within w_info_resumen_inspeccion
end type
type uo_selespe from uo_seleccion_especie_mod within w_info_resumen_inspeccion
end type
type uo_selemba from uo_seleccion_embalajesprod_mod within w_info_resumen_inspeccion
end type
type uo_selpack from uo_seleccion_plantapacking_mod within w_info_resumen_inspeccion
end type
type uo_selfrig from uo_seleccion_frigorifico_mod within w_info_resumen_inspeccion
end type
type dw_1 from uo_dw within w_info_resumen_inspeccion
end type
end forward

global type w_info_resumen_inspeccion from w_para_informes
integer x = 14
integer y = 32
integer width = 3141
integer height = 2044
string title = "Resolucion de Lotes"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
em_fechadesde em_fechadesde
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_12 st_12
st_3 st_3
st_5 st_5
st_6 st_6
cbx_filtro cbx_filtro
ddlb_filtro ddlb_filtro
st_8 st_8
st_4 st_4
st_2 st_2
st_7 st_7
gb_5 gb_5
st_9 st_9
gb_4 gb_4
sle_1 sle_1
uo_selzona uo_selzona
uo_selvari uo_selvari
uo_selprod uo_selprod
uo_selespe uo_selespe
uo_selemba uo_selemba
uo_selpack uo_selpack
uo_selfrig uo_selfrig
dw_1 dw_1
end type
global w_info_resumen_inspeccion w_info_resumen_inspeccion

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report

DataWindowChild		idwc_zona, idwc_planta, idwc_cliente, idwc_tecnico,idwc_especie,idwc_prod,idwc_emba,idwc_variedad
uo_zonas					iuo_zonas
uo_ctlcalagronomos	iuo_ctlcalagronomos
uo_especie				iuo_especie
uo_productores       iuo_productores
uo_embalajesprod     iuo_embalajesprod
uo_variedades        iuo_variedades

end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexistetecnico (integer ai_tecnico)
public function boolean noexisteagroproduc (integer ai_agronomo)
public function boolean filtroespecie (integer ai_especie)
public function string sacaplanta (integer li_codplanta)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_zona

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

public function boolean noexistetecnico (integer ai_tecnico);/*Integer li_Contador, li_zona, li_tecnico

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.ctlcalagronomos
	WHERE	ccag_codigo = :ai_tecnico
	AND	:li_tecnico in (0,ccag_codigo )
	AND	:li_zona in (0,zona_codigo);

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Agronómo No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE
END IF
*/
return false
end function

public function boolean noexisteagroproduc (integer ai_agronomo);/*Integer li_Contador, li_zona
Long    ll_prod

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.ctlcalagroproduc
	WHERE	ccag_codigo = :ai_agronomo
	AND	:ll_prod in (0,prod_codigo )
	AND	:li_zona in (0,zona_codigo);

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Agrónomo No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE
END IF
*/
return false

end function

public function boolean filtroespecie (integer ai_especie);//return
String ls_Nula, ls_Filtro, ls_Letra
SetNull(ls_Nula)
/*
IF iuo_especie.existe(ai_especie,True,sqlca) = False THEN
	dw_especie.SetItem(1, "espe_codigo", Long(ls_nula))
	RETURN True
ELSE
	dw_variedad.GetChild("vari_codigo", idwc_variedad)
   idwc_variedad.SetTransObject(sqlca)
   idwc_variedad.Retrieve(ai_especie)
   dw_variedad.InsertRow(0)
	
	ls_Letra		=	Mid(iuo_especie.nombre,1,1)
	ls_Filtro	=	"Mid(emba_codigo,1,1) =  '" + ls_Letra + "'" 
	
	idwc_emba.SetFilter("")
	idwc_emba.Filter()
	
	idwc_emba.SetFilter(ls_Filtro)
	idwc_emba.Filter()
	
	RETURN False
END IF
*/
return false
end function

public function string sacaplanta (integer li_codplanta);String ls_planta

Select		IsNull(plde_nombre, ' ')
	Into		:ls_planta
	From		dbo.plantadesp
	Where	plde_codigo = :li_codplanta
	Using 		Sqlca;

If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura tabla ctlcalreclamosenca  ")
	Return ' '
Else
	Return ls_planta
End If
end function

on w_info_resumen_inspeccion.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fechadesde=create em_fechadesde
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_12=create st_12
this.st_3=create st_3
this.st_5=create st_5
this.st_6=create st_6
this.cbx_filtro=create cbx_filtro
this.ddlb_filtro=create ddlb_filtro
this.st_8=create st_8
this.st_4=create st_4
this.st_2=create st_2
this.st_7=create st_7
this.gb_5=create gb_5
this.st_9=create st_9
this.gb_4=create gb_4
this.sle_1=create sle_1
this.uo_selzona=create uo_selzona
this.uo_selvari=create uo_selvari
this.uo_selprod=create uo_selprod
this.uo_selespe=create uo_selespe
this.uo_selemba=create uo_selemba
this.uo_selpack=create uo_selpack
this.uo_selfrig=create uo_selfrig
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fechadesde
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_12
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.cbx_filtro
this.Control[iCurrent+13]=this.ddlb_filtro
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.st_4
this.Control[iCurrent+16]=this.st_2
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.gb_5
this.Control[iCurrent+19]=this.st_9
this.Control[iCurrent+20]=this.gb_4
this.Control[iCurrent+21]=this.sle_1
this.Control[iCurrent+22]=this.uo_selzona
this.Control[iCurrent+23]=this.uo_selvari
this.Control[iCurrent+24]=this.uo_selprod
this.Control[iCurrent+25]=this.uo_selespe
this.Control[iCurrent+26]=this.uo_selemba
this.Control[iCurrent+27]=this.uo_selpack
this.Control[iCurrent+28]=this.uo_selfrig
this.Control[iCurrent+29]=this.dw_1
end on

on w_info_resumen_inspeccion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fechadesde)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_12)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.cbx_filtro)
destroy(this.ddlb_filtro)
destroy(this.st_8)
destroy(this.st_4)
destroy(this.st_2)
destroy(this.st_7)
destroy(this.gb_5)
destroy(this.st_9)
destroy(this.gb_4)
destroy(this.sle_1)
destroy(this.uo_selzona)
destroy(this.uo_selvari)
destroy(this.uo_selprod)
destroy(this.uo_selespe)
destroy(this.uo_selemba)
destroy(this.uo_selpack)
destroy(this.uo_selfrig)
destroy(this.dw_1)
end on

event open;call super::open;String ls_Filtro, ls_Nula
x = 0
y = 0

SetNull(ls_Nula)
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

If IsNull(uo_selzona.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_selfrig.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_selpack.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_selprod.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_selespe.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_selvari.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_selemba.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelZona.Seleccion(True, False)
	uo_selfrig.Seleccion(True, True)
	uo_selprod.Seleccion(True, False)
	uo_selpack.Seleccion(True, True)
	uo_selpack.cbx_consolida.checked = True
	uo_selespe.Seleccion(False, False)
	uo_selvari.Seleccion(True, False)
	uo_selemba.Seleccion(True, False)
	
	uo_selpack.cbx_consolida.triggerevent('clicked')

// Inicia en UVAS
	uo_selespe.dw_seleccion.Object.codigo[1] = 21
	uo_selespe.iuo_Especie.Existe(21, False, Sqlca)
	uo_selespe.Codigo 	= 21
	uo_selespe.Nombre	= uo_selespe.iuo_Especie.Nombre
	uo_selvari.filtra(21)
	uo_SelEmba.Filtra(gi_CodExport, uo_SelEspe.Codigo)
	
	em_fechadesde.text	=	String(Today())
	em_fechahasta.text	=	String(Today())
	
End If

end event

event resize;call super::resize;pb_Excel.x			=	pb_acepta.x
pb_Excel.y			=	pb_acepta.y - 240
pb_Excel.width		=	235
pb_Excel.height		=	195
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_inspeccion
integer x = 2702
integer y = 244
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_inspeccion
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_inspeccion
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_inspeccion
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_inspeccion
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_inspeccion
integer width = 2373
string text = "Informe Resolución de Lotes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_inspeccion
string tag = "Imprimir Reporte"
integer x = 2734
integer y = 696
integer taborder = 210
end type

event pb_acepta::clicked;Integer	li_fila, li_planta, li_Tecnico, li_Tipo, li_zona, li_especie, li_vari
Long     ll_prod
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_filtrolotes,ls_emba


If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

If ddlb_filtro.Text = 'Aprobados' Then
	 ls_filtrolotes  = 'A'
	 
ElseIf ddlb_filtro.Text = 'Objetados' Then
	ls_filtrolotes  = 'O'
ElseIf ddlb_filtro.Text = "Aprobados Comercial" Then
	 ls_filtrolotes  = 'Z'
Else 
	ls_filtrolotes   = '*'	
End If	 

istr_info.titulo	= 'INFORME GRAFICO DE LOTES'

If uo_selespe.codigo = 11 Then
   OpenWithParm(vinf,istr_info)
   vinf.dw_1.DataObject = "dw_info_resumen_inspeccion1"
	vinf.dw_1.Object.DataWindow.Zoom = 81

ElseIf uo_selespe.codigo <> 11 Then
	OpenWithParm(vinf,istr_info)
	vinf.dw_1.DataObject = "dw_info_resumen_inspecciones"
	vinf.dw_1.Object.DataWindow.Zoom = 85 //95
End If

datawindowchild idwc_destino1

vinf.dw_1.GetChild("cctd_repesa_1", idwc_destino1)
idwc_destino1.SetTransObject(sqlca)
idwc_destino1.Retrieve(gi_CodEspecie)

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_codexport, uo_selzona.codigo,uo_selfrig.codigo, uo_selpack.codigo, ld_FechaEmbaini, ld_FechaEmbafin,&
									uo_selespe.codigo, uo_selvari.codigo, -1,uo_selprod.codigo,ls_filtrolotes,uo_selemba.codigo)

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	vinf.dw_1.GroupCalc()
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_inspeccion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2729
integer y = 976
integer taborder = 220
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 688
integer width = 311
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
string text = "Frigorífico"
boolean focusrectangle = false
end type

type em_fechadesde from editmask within w_info_resumen_inspeccion
integer x = 901
integer y = 1588
integer width = 494
integer height = 92
integer taborder = 190
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

type st_13 from statictext within w_info_resumen_inspeccion
integer x = 1518
integer y = 1524
integer width = 434
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

type em_fechahasta from editmask within w_info_resumen_inspeccion
integer x = 1518
integer y = 1588
integer width = 494
integer height = 92
integer taborder = 200
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

type st_zona from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 580
integer width = 462
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 788
integer width = 315
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
string text = "Packing"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_resumen_inspeccion
integer x = 2034
integer y = 1584
integer width = 101
integer height = 92
integer taborder = 180
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

type st_12 from statictext within w_info_resumen_inspeccion
integer x = 951
integer y = 1524
integer width = 434
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

type st_3 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 1000
integer width = 238
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

type st_5 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 896
integer width = 297
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 1340
integer width = 507
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
string text = "Resolución Lotes"
boolean focusrectangle = false
end type

type cbx_filtro from checkbox within w_info_resumen_inspeccion
integer x = 2034
integer y = 1320
integer width = 101
integer height = 92
integer taborder = 160
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
	ddlb_filtro.Text = ''	
	ddlb_filtro.Enabled = FALSE	
   
ELSE
	ddlb_filtro.Enabled = True
	ddlb_filtro.Text    = 'Aprobados'
END IF
RETURN 0

end event

type ddlb_filtro from dropdownlistbox within w_info_resumen_inspeccion
integer x = 919
integer y = 1320
integer width = 869
integer height = 308
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean allowedit = true
string item[] = {"Aprobados","Objetados","Aprobados Comercial"}
borderstyle borderstyle = stylelowered!
end type

type st_8 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 1216
integer width = 297
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

type st_4 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 1584
integer width = 576
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
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumen_inspeccion
integer x = 1778
integer y = 508
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
string text = "Todos"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumen_inspeccion
integer x = 370
integer y = 1108
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
string text = "Variedad"
boolean focusrectangle = false
end type

type gb_5 from groupbox within w_info_resumen_inspeccion
integer x = 329
integer y = 1480
integer width = 2222
integer height = 240
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_9 from statictext within w_info_resumen_inspeccion
integer x = 1970
integer y = 508
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
string text = "Consolidado"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_resumen_inspeccion
integer x = 329
integer y = 448
integer width = 2222
integer height = 1008
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
end type

type sle_1 from singlelineedit within w_info_resumen_inspeccion
integer x = 251
integer y = 436
integer width = 2373
integer height = 1320
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
borderstyle borderstyle = styleraised!
end type

type uo_selzona from uo_seleccion_zonas_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 914
integer y = 552
integer width = 1266
integer taborder = 30
boolean bringtotop = true
end type

on uo_selzona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_selvari from uo_seleccion_variedad_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 914
integer y = 1084
integer taborder = 230
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selprod from uo_seleccion_productor_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 914
integer y = 872
integer width = 1280
integer taborder = 70
boolean bringtotop = true
end type

on uo_selprod.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_selespe from uo_seleccion_especie_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 914
integer y = 976
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespe.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVari.Filtra(This.Codigo)
		uo_SelEmba.Filtra(gi_codexport, This.Codigo)
		
End Choose
end event

type uo_selemba from uo_seleccion_embalajesprod_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 919
integer y = 1188
integer taborder = 120
boolean bringtotop = true
end type

on uo_selemba.destroy
call uo_seleccion_embalajesprod_mod::destroy
end on

type uo_selpack from uo_seleccion_plantapacking_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 914
integer y = 764
integer taborder = 40
boolean bringtotop = true
end type

on uo_selpack.destroy
call uo_seleccion_plantapacking_mod::destroy
end on

type uo_selfrig from uo_seleccion_frigorifico_mod within w_info_resumen_inspeccion
event destroy ( )
integer x = 914
integer y = 656
integer taborder = 40
boolean bringtotop = true
end type

on uo_selfrig.destroy
call uo_seleccion_frigorifico_mod::destroy
end on

event clicked;Integer	li_fila, li_planta, li_Tecnico, li_Tipo, li_zona, li_especie, li_vari
Long     ll_prod
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_filtrolotes,ls_emba,ls_ruta,ls_archivo,ls_planta

SetPointer(HourGlass!)
RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

If ddlb_filtro.Text = 'Aprobados' Then
	ls_filtrolotes  = 'A'
ElseIf ddlb_filtro.Text = 'Objetados' Then
	ls_filtrolotes  = 'O'
ElseIf ddlb_filtro.Text = "Aprobados Comercial" Then
	ls_filtrolotes  = 'Z'
Else 
	ls_filtrolotes   = '*'	
End If	 

ls_planta = sacaplanta(gi_codplanta)
ls_Archivo = '\RL_' + uo_SelEspe.Nombre +'_'+ mid(string(today()),1,2)+mid(string(today()),4,2)+mid(string(today()),7,4) +'_' + trim(ls_planta)+ '.xls'
dw_1.DataObject = "dw_info_resumen_inspeccion_excel"	
dw_1.SetTransObject(sqlca)
	
li_fila = dw_1.Retrieve(gi_codexport, uo_selzona.codigo,uo_selfrig.codigo, uo_selpack.codigo, ld_FechaEmbaini, ld_FechaEmbafin,&
  							  uo_selespe.codigo, uo_selvari.codigo, -1,uo_selprod.codigo,ls_filtrolotes,uo_selemba.codigo)		

If li_fila = -1 Then
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este Archivo.", &
					StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, Excel8!, True) = 1 Then
		MessageBox('Atencion', 'Se genero archivo ' + ls_Archivo)
	Else
		MessageBox('Atencion', 'No se pudo generar archivo ' + ls_Archivo)		
	End If	
End If

SetPointer(Arrow!)

end event

type dw_1 from uo_dw within w_info_resumen_inspeccion
boolean visible = false
integer x = -78
integer y = -728
integer width = 229
integer height = 172
integer taborder = 10
boolean bringtotop = true
end type

