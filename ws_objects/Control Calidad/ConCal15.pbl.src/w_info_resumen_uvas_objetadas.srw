$PBExportHeader$w_info_resumen_uvas_objetadas.srw
$PBExportComments$Ventana de Informe Revisión Inspección
forward
global type w_info_resumen_uvas_objetadas from w_para_informes
end type
type st_1 from statictext within w_info_resumen_uvas_objetadas
end type
type em_fechadesde from editmask within w_info_resumen_uvas_objetadas
end type
type st_13 from statictext within w_info_resumen_uvas_objetadas
end type
type em_fechahasta from editmask within w_info_resumen_uvas_objetadas
end type
type st_zona from statictext within w_info_resumen_uvas_objetadas
end type
type dw_zona from datawindow within w_info_resumen_uvas_objetadas
end type
type cbx_todoplanta from checkbox within w_info_resumen_uvas_objetadas
end type
type dw_planta from datawindow within w_info_resumen_uvas_objetadas
end type
type st_33 from statictext within w_info_resumen_uvas_objetadas
end type
type dw_tecnico from datawindow within w_info_resumen_uvas_objetadas
end type
type cbx_todoszona from checkbox within w_info_resumen_uvas_objetadas
end type
type cbx_todosagro from checkbox within w_info_resumen_uvas_objetadas
end type
type cbx_todosfecha from checkbox within w_info_resumen_uvas_objetadas
end type
type st_12 from statictext within w_info_resumen_uvas_objetadas
end type
type dw_especie from datawindow within w_info_resumen_uvas_objetadas
end type
type st_3 from statictext within w_info_resumen_uvas_objetadas
end type
type st_5 from statictext within w_info_resumen_uvas_objetadas
end type
type cbx_todoproduc from checkbox within w_info_resumen_uvas_objetadas
end type
type dw_prod from datawindow within w_info_resumen_uvas_objetadas
end type
type gb_5 from groupbox within w_info_resumen_uvas_objetadas
end type
type cbx_todoemba from checkbox within w_info_resumen_uvas_objetadas
end type
type dw_emba from datawindow within w_info_resumen_uvas_objetadas
end type
type st_8 from statictext within w_info_resumen_uvas_objetadas
end type
type st_4 from statictext within w_info_resumen_uvas_objetadas
end type
type st_2 from statictext within w_info_resumen_uvas_objetadas
end type
type cbx_1 from checkbox within w_info_resumen_uvas_objetadas
end type
type cbx_2 from checkbox within w_info_resumen_uvas_objetadas
end type
type cbx_3 from checkbox within w_info_resumen_uvas_objetadas
end type
type cbx_4 from checkbox within w_info_resumen_uvas_objetadas
end type
type cbx_5 from checkbox within w_info_resumen_uvas_objetadas
end type
type st_6 from statictext within w_info_resumen_uvas_objetadas
end type
type gb_4 from groupbox within w_info_resumen_uvas_objetadas
end type
type gb_3 from groupbox within w_info_resumen_uvas_objetadas
end type
type sle_1 from singlelineedit within w_info_resumen_uvas_objetadas
end type
end forward

global type w_info_resumen_uvas_objetadas from w_para_informes
integer x = 14
integer y = 32
integer width = 2949
integer height = 1524
string title = "Resolución Gráfica de Lotes"
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
dw_zona dw_zona
cbx_todoplanta cbx_todoplanta
dw_planta dw_planta
st_33 st_33
dw_tecnico dw_tecnico
cbx_todoszona cbx_todoszona
cbx_todosagro cbx_todosagro
cbx_todosfecha cbx_todosfecha
st_12 st_12
dw_especie dw_especie
st_3 st_3
st_5 st_5
cbx_todoproduc cbx_todoproduc
dw_prod dw_prod
gb_5 gb_5
cbx_todoemba cbx_todoemba
dw_emba dw_emba
st_8 st_8
st_4 st_4
st_2 st_2
cbx_1 cbx_1
cbx_2 cbx_2
cbx_3 cbx_3
cbx_4 cbx_4
cbx_5 cbx_5
st_6 st_6
gb_4 gb_4
gb_3 gb_3
sle_1 sle_1
end type
global w_info_resumen_uvas_objetadas w_info_resumen_uvas_objetadas

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report

DataWindowChild		idwc_zona, idwc_planta, idwc_cliente, idwc_tecnico,idwc_especie,idwc_prod,idwc_emba
uo_zonas					iuo_zonas
uo_ctlcaltecnicos		iuo_ctlcaltecnicos
uo_especie				iuo_especie
uo_productores       iuo_productores
uo_embalajesprod         iuo_embalajes

end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexistetecnico (integer ai_tecnico)
public function boolean noexisteagroproduc (integer ai_agronomo)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_cliente,li_zona

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.plantadesp
	WHERE plde_codigo = :planta
	AND	plde_tipopl	= :Tipo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	IF Tipo = 1 THEN
		Messagebox("Atención","Código Planta No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	ELSE		
		Messagebox("Atención","Código Packing No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	END IF					
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

public function boolean noexistetecnico (integer ai_tecnico);Integer li_Contador, li_zona, li_tecnico

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.ctlcalagronomos
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

end function

public function boolean noexisteagroproduc (integer ai_agronomo);Integer li_Contador, li_zona
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
	messagebox("Atención","Código Agrónomo No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE
END IF

end function

on w_info_resumen_uvas_objetadas.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fechadesde=create em_fechadesde
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.dw_zona=create dw_zona
this.cbx_todoplanta=create cbx_todoplanta
this.dw_planta=create dw_planta
this.st_33=create st_33
this.dw_tecnico=create dw_tecnico
this.cbx_todoszona=create cbx_todoszona
this.cbx_todosagro=create cbx_todosagro
this.cbx_todosfecha=create cbx_todosfecha
this.st_12=create st_12
this.dw_especie=create dw_especie
this.st_3=create st_3
this.st_5=create st_5
this.cbx_todoproduc=create cbx_todoproduc
this.dw_prod=create dw_prod
this.gb_5=create gb_5
this.cbx_todoemba=create cbx_todoemba
this.dw_emba=create dw_emba
this.st_8=create st_8
this.st_4=create st_4
this.st_2=create st_2
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.cbx_4=create cbx_4
this.cbx_5=create cbx_5
this.st_6=create st_6
this.gb_4=create gb_4
this.gb_3=create gb_3
this.sle_1=create sle_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fechadesde
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.dw_zona
this.Control[iCurrent+7]=this.cbx_todoplanta
this.Control[iCurrent+8]=this.dw_planta
this.Control[iCurrent+9]=this.st_33
this.Control[iCurrent+10]=this.dw_tecnico
this.Control[iCurrent+11]=this.cbx_todoszona
this.Control[iCurrent+12]=this.cbx_todosagro
this.Control[iCurrent+13]=this.cbx_todosfecha
this.Control[iCurrent+14]=this.st_12
this.Control[iCurrent+15]=this.dw_especie
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.cbx_todoproduc
this.Control[iCurrent+19]=this.dw_prod
this.Control[iCurrent+20]=this.gb_5
this.Control[iCurrent+21]=this.cbx_todoemba
this.Control[iCurrent+22]=this.dw_emba
this.Control[iCurrent+23]=this.st_8
this.Control[iCurrent+24]=this.st_4
this.Control[iCurrent+25]=this.st_2
this.Control[iCurrent+26]=this.cbx_1
this.Control[iCurrent+27]=this.cbx_2
this.Control[iCurrent+28]=this.cbx_3
this.Control[iCurrent+29]=this.cbx_4
this.Control[iCurrent+30]=this.cbx_5
this.Control[iCurrent+31]=this.st_6
this.Control[iCurrent+32]=this.gb_4
this.Control[iCurrent+33]=this.gb_3
this.Control[iCurrent+34]=this.sle_1
end on

on w_info_resumen_uvas_objetadas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fechadesde)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.dw_zona)
destroy(this.cbx_todoplanta)
destroy(this.dw_planta)
destroy(this.st_33)
destroy(this.dw_tecnico)
destroy(this.cbx_todoszona)
destroy(this.cbx_todosagro)
destroy(this.cbx_todosfecha)
destroy(this.st_12)
destroy(this.dw_especie)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.cbx_todoproduc)
destroy(this.dw_prod)
destroy(this.gb_5)
destroy(this.cbx_todoemba)
destroy(this.dw_emba)
destroy(this.st_8)
destroy(this.st_4)
destroy(this.st_2)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.cbx_4)
destroy(this.cbx_5)
destroy(this.st_6)
destroy(this.gb_4)
destroy(this.gb_3)
destroy(this.sle_1)
end on

event open;string ls_Filtro
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

iuo_zonas				=  Create 	uo_zonas
iuo_ctlcaltecnicos	=  Create 	uo_ctlcaltecnicos
iuo_especie				=	Create	uo_especie
iuo_productores      =  Create   uo_productores 
iuo_embalajes        =  Create   uo_embalajesprod 

//zona
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_zona.SetItem(1,"zona_codigo", 6)

//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1,-1)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",12)

//Técnicos
dw_tecnico.GetChild("ccag_codigo", idwc_tecnico)
idwc_tecnico.SetTransObject(sqlca)
idwc_tecnico.Retrieve(0)
dw_tecnico.InsertRow(0)

//Especie
dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_Especie.SetItem(1,"espe_codigo", 11)

em_fechadesde.text	=	String(Today())
em_fechahasta.text	=	String(Today())

//Productor
dw_prod.GetChild("prod_codigo", idwc_prod)
idwc_prod.SetTransObject(sqlca)
idwc_prod.Retrieve()
dw_prod.InsertRow(0)
dw_prod.SetItem(1,"prod_codigo",176)

//Embalajes
dw_emba.GetChild("emba_codigo", idwc_emba)
idwc_emba.SetTransObject(sqlca)
idwc_emba.Retrieve(gi_codexport)
dw_emba.InsertRow(0)

ls_Filtro = "Mid(emba_codigo,1,1) = 'U'" 
idwc_emba.SetFilter(ls_Filtro)
idwc_emba.Filter()
end event

type st_computador from w_para_informes`st_computador within w_info_resumen_uvas_objetadas
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_uvas_objetadas
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_uvas_objetadas
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_uvas_objetadas
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_uvas_objetadas
integer x = 78
integer y = 32
integer width = 2418
string text = "Informe Resolución Gráfica de Lotes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_uvas_objetadas
string tag = "Imprimir Reporte"
integer x = 2615
integer y = 352
integer taborder = 200
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_planta, li_Tecnico, li_Tipo, li_zona, li_especie,li_variedad
Long     ll_prod
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_emba, ls_filtro
SetPointer(HourGlass!)

//Variedad
li_variedad=9999
ls_filtro='*'

//Zona
IF cbx_1.Checked THEN
	li_zona = -9
ELSE
  IF cbx_todoszona.Checked THEN
	  li_zona	= -1
  ELSE
	  li_zona	= dw_zona.Object.zona_codigo[1]
	  IF IsNull(li_zona)THEN
	     MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
		  RETURN
	  END IF
  END IF
END IF

//Planta
IF cbx_2.Checked THEN
	li_planta = -9
ELSE
  IF cbx_todoplanta.Checked THEN
	  li_planta	 = -1
  ELSE
	  li_planta	 = dw_planta.Object.plde_codigo[1]
	  IF IsNull(li_planta)THEN
	     MessageBox("Atención","Debe Seleccionar una Planta Previamente",Exclamation!)
		  RETURN
	  END IF
  END IF
END IF

//Agronomo
IF cbx_5.Checked THEN
	li_tecnico  = -9
ELSE
  IF cbx_todosagro.Checked THEN
	  li_Tecnico	 = -1
  ELSE
	  li_Tecnico	= dw_tecnico.Object.ccag_codigo[1]
	  IF IsNull(li_Tecnico)THEN
		  MessageBox("Atención","Debe Seleccionar un Agronomo Previamente",Exclamation!)
		  RETURN
	  END IF
  END IF
END IF

//Productor
IF cbx_3.Checked THEN
	ll_prod   = -9
ELSE
  IF cbx_todoproduc.Checked THEN
	  ll_prod	 = -1
  ELSE
	  ll_prod	= dw_prod.Object.prod_codigo[1]
	  IF IsNull(ll_prod) THEN
		  MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
		  RETURN
	  END IF
  END IF
END IF

//Especie
li_especie	= dw_especie.Object.espe_codigo[1]
IF IsNull(li_especie)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	RETURN
END IF

//Rango de fecha
IF cbx_todosfecha.Checked THEN
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
ELSE
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
END IF

//Embalaje
IF cbx_4.Checked THEN
	ls_emba = '**'
ELSE
  IF cbx_todoemba.Checked THEN
	  ls_emba	 = '*'
  ELSE
	  ls_emba	= dw_emba.Object.emba_codigo[1]
	  IF IsNull(ls_emba) THEN
		  MessageBox("Atención","Debe Seleccionar un Embalaje Previamente",Exclamation!)
		  RETURN
	  END IF
  END IF
END IF

istr_info.titulo	= 'INFORME RESOLUCIÓN GRAFICA DE LOTES'

OpenWithParm(vinf,istr_info)
IF li_especie = 11 THEN
   vinf.dw_1.DataObject = "dw_info_resumen_uvas_objetadas"
ELSE
	vinf.dw_1.DataObject = "dw_info_resumen_especies_objetadas"
END IF
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_codexport,li_zona, li_Planta, ld_FechaEmbaini, &
									  ld_FechaEmbafin,li_especie,li_variedad,li_Tecnico, &
									  ll_prod,ls_filtro,ls_emba)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	vinf.dw_1.GroupCalc()
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 96
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_uvas_objetadas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2610
integer y = 632
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 428
integer width = 311
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Frigorífico"
boolean focusrectangle = false
end type

type em_fechadesde from editmask within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 1048
integer width = 370
integer height = 92
integer taborder = 180
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

type st_13 from statictext within w_info_resumen_uvas_objetadas
integer x = 1234
integer y = 992
integer width = 178
integer height = 52
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_resumen_uvas_objetadas
integer x = 1234
integer y = 1048
integer width = 370
integer height = 92
integer taborder = 190
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

type st_zona from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 320
integer width = 462
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Zona Origen"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 292
integer width = 827
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_zona.PostEvent(Clicked!)

IF iuo_zonas.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "zona_codigo", Long(ls_nula))
	RETURN 1
ELSE
	
	dw_tecnico.GetChild("ccag_codigo", idwc_tecnico)
	idwc_tecnico.SetTransObject(sqlca)
	idwc_tecnico.Retrieve(Integer(Data))
	dw_tecnico.SetItem(1, "ccag_codigo", Long(ls_Nula))
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1,Integer(Data))
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	dw_planta.InsertRow(0)
	
	RETURN 0
END IF	
end event

event itemerror;Return 1
end event

event clicked;cbx_todoszona.Checked = False
cbx_1.Checked = False
end event

type cbx_todoplanta from checkbox within w_info_resumen_uvas_objetadas
integer x = 1829
integer y = 400
integer width = 101
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	//dw_planta.Enabled = FALSE	
	cbx_2.Checked = False
   dw_planta.SetItem(1, "plde_codigo", li_null)
ELSE
	//dw_planta.Enabled 		=  TRUE
	dw_planta.Setfocus()
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
	
END IF
RETURN 0

end event

type dw_planta from datawindow within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 400
integer width = 960
integer height = 92
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_planta.PostEvent(Clicked!)

IF NoExistePlanta(Integer(data),1) THEN
	This.SetItem(1, "plde_codigo", Long(ls_nula))
	RETURN 1
END IF	

end event

event itemerror;RETURN 1
end event

event clicked;cbx_todoplanta.Checked = False
cbx_2.Checked = False
end event

type st_33 from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 852
integer width = 315
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Agrónomo"
boolean focusrectangle = false
end type

type dw_tecnico from datawindow within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 832
integer width = 1024
integer height = 92
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_ctlcalagronomos"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

event itemchanged;String	ls_Nula
SetNull(ls_Nula)

dw_tecnico.PostEvent(Clicked!)

IF NoExisteagroproduc(Integer(data)) THEN
    This.SetItem(1,"ccag_codigo", Long(ls_nula))
    RETURN 1
END IF	

end event

event clicked;cbx_todosagro.Checked = False
cbx_5.Checked = False
end event

type cbx_todoszona from checkbox within w_info_resumen_uvas_objetadas
integer x = 1829
integer y = 292
integer width = 101
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	//dw_zona.Enabled = False	
	dw_zona.SetItem(1, "zona_codigo", li_null)
	cbx_1.Checked = False
	/*Planta*/
	//idwc_planta.Retrieve(gi_codexport,1,0)
	//idwc_planta.SetSort("plde_nombre A")
	//idwc_planta.Sort()
	/*Agronomo*/
	//idwc_tecnico.Retrieve(0,0)
ELSE
	//dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	//idwc_planta.Retrieve(gi_codexport,1,gi_codzona)
	//idwc_planta.SetSort("plde_nombre A")
	//idwc_planta.Sort()
	
	/*Agronomo*/
	//idwc_tecnico.Retrieve(0,gi_codzona)
	//dw_zona.Enabled 		=  True
	dw_zona.Setfocus()
END IF
RETURN 0

end event

type cbx_todosagro from checkbox within w_info_resumen_uvas_objetadas
integer x = 1829
integer y = 832
integer width = 101
integer height = 92
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_tecnico.Enabled	=	FALSE	
	cbx_5.Checked = False
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
ELSE
	//dw_tecnico.Enabled	=	TRUE
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
	dw_tecnico.Setfocus()
END IF
end event

type cbx_todosfecha from checkbox within w_info_resumen_uvas_objetadas
integer x = 1829
integer y = 1048
integer width = 101
integer height = 92
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
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

type st_12 from statictext within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 992
integer width = 201
integer height = 52
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 616
integer width = 859
integer height = 92
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula, ls_Filtro, ls_Letra
SetNull(ls_Nula)

IF iuo_especie.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "espe_codigo", Long(ls_nula))
	RETURN 1
ELSE
	ls_Letra		=	Mid(iuo_especie.nombre,1,1)
	ls_Filtro	=	"Mid(emba_codigo,1,1) =  '" + ls_Letra + "'" 
	
	idwc_emba.SetFilter("")
	idwc_emba.Filter()
	
	idwc_emba.SetFilter(ls_Filtro)
	idwc_emba.Filter()
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 636
integer width = 238
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 528
integer width = 297
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_todoproduc from checkbox within w_info_resumen_uvas_objetadas
integer x = 1829
integer y = 508
integer width = 101
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;Long	ll_null

SetNull(ll_Null)

IF This.Checked THEN
	//dw_prod.Enabled = FALSE	
	cbx_3.Checked = False
   dw_prod.SetItem(1, "prod_codigo", ll_null)
ELSE
	//dw_prod.Enabled 	=  TRUE
	dw_prod.Setfocus()
	dw_prod.SetItem(1, "prod_codigo", ll_null)
	
END IF
RETURN 0

end event

type dw_prod from datawindow within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 508
integer width = 965
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_prod.PostEvent(Clicked!)

IF iuo_productores.existe(Long(Data),True,sqlca) = False THEN
	This.SetItem(1, "prod_codigo", Long(ls_nula))
	RETURN 1
END IF	

end event

event itemerror;RETURN 1
end event

event clicked;cbx_todoproduc.Checked = False
cbx_3.Checked = False
end event

type gb_5 from groupbox within w_info_resumen_uvas_objetadas
integer x = 187
integer y = 204
integer width = 2190
integer height = 756
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 12632256
end type

type cbx_todoemba from checkbox within w_info_resumen_uvas_objetadas
integer x = 1829
integer y = 724
integer width = 101
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;String	ls_null

SetNull(ls_Null)

IF This.Checked THEN
	//dw_emba.Enabled = FALSE	
	cbx_4.Checked = False
   dw_emba.SetItem(1, "emba_codigo", ls_null)
ELSE
	//dw_emba.Enabled 	=  TRUE
	dw_emba.Setfocus()
	dw_emba.SetItem(1, "emba_codigo", ls_null)
	
END IF
RETURN 0

end event

type dw_emba from datawindow within w_info_resumen_uvas_objetadas
integer x = 709
integer y = 724
integer width = 969
integer height = 92
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_embalajesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_emba.PostEvent(Clicked!)

IF iuo_embalajes.existe(gi_codexport,Data,True,sqlca) = False THEN
	This.SetItem(1, "Emba_codigo", ls_nula)
	RETURN 1
END IF	

end event

event itemerror;Return 1
end event

event clicked;cbx_todoemba.Checked = False
cbx_4.Checked = False
end event

type st_8 from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 744
integer width = 297
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_resumen_uvas_objetadas
integer x = 206
integer y = 1048
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumen_uvas_objetadas
integer x = 1760
integer y = 248
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_info_resumen_uvas_objetadas
integer x = 2126
integer y = 292
integer width = 101
integer height = 92
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	//dw_zona.Enabled = False	
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	//idwc_planta.Retrieve(gi_codexport,1,0)
	//idwc_planta.SetSort("plde_nombre A")
	//idwc_planta.Sort()
	cbx_todoszona.Checked = False
	/*Agronomo*/
	idwc_tecnico.Retrieve(0,0)
ELSE
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(gi_codexport,1,gi_codzona)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	
	/*Agronomo*/
	idwc_tecnico.Retrieve(0,gi_codzona)
	//dw_zona.Enabled 		=  True
	dw_zona.Setfocus()
END IF
RETURN 0

end event

type cbx_2 from checkbox within w_info_resumen_uvas_objetadas
integer x = 2126
integer y = 400
integer width = 101
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	//dw_planta.Enabled = FALSE	
   dw_planta.SetItem(1, "plde_codigo", li_null)
	cbx_todoplanta.Checked = False
ELSE
	//dw_planta.Enabled 		=  TRUE
	dw_planta.Setfocus()
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
	
END IF
RETURN 0

end event

type cbx_3 from checkbox within w_info_resumen_uvas_objetadas
integer x = 2126
integer y = 508
integer width = 101
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;Long	ll_null

SetNull(ll_Null)

IF This.Checked THEN
	//dw_prod.Enabled = FALSE	
	cbx_todoproduc.Checked = False
   dw_prod.SetItem(1, "prod_codigo", ll_null)
ELSE
	//dw_prod.Enabled 	=  TRUE
	dw_prod.Setfocus()
	dw_prod.SetItem(1, "prod_codigo", ll_null)
	
END IF
RETURN 0

end event

type cbx_4 from checkbox within w_info_resumen_uvas_objetadas
integer x = 2126
integer y = 724
integer width = 101
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;String	ls_null

SetNull(ls_Null)

IF This.Checked THEN
	//dw_emba.Enabled = FALSE	
	cbx_todoemba.Checked = False
   dw_emba.SetItem(1, "emba_codigo", ls_null)
ELSE
	//dw_emba.Enabled 	=  TRUE
	dw_emba.Setfocus()
	dw_emba.SetItem(1, "emba_codigo", ls_null)
	
END IF
RETURN 0

end event

type cbx_5 from checkbox within w_info_resumen_uvas_objetadas
integer x = 2126
integer y = 832
integer width = 101
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_tecnico.Enabled	=	FALSE	
	cbx_todosagro.Checked = False
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
ELSE
	//dw_tecnico.Enabled	=	TRUE
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
	dw_tecnico.Setfocus()
END IF
end event

type st_6 from statictext within w_info_resumen_uvas_objetadas
integer x = 1998
integer y = 248
integer width = 375
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidado"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_resumen_uvas_objetadas
integer x = 192
integer y = 952
integer width = 2176
integer height = 244
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_3 from groupbox within w_info_resumen_uvas_objetadas
integer x = 142
integer y = 156
integer width = 2281
integer height = 1088
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 12632256
end type

type sle_1 from singlelineedit within w_info_resumen_uvas_objetadas
integer x = 78
integer y = 144
integer width = 2418
integer height = 1140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
borderstyle borderstyle = styleraised!
end type

