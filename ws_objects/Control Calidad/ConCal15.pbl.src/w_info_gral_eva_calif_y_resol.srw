$PBExportHeader$w_info_gral_eva_calif_y_resol.srw
$PBExportComments$Ventana de Informe General de Evaluación y Calificación.
forward
global type w_info_gral_eva_calif_y_resol from w_para_informes
end type
type gb_7 from groupbox within w_info_gral_eva_calif_y_resol
end type
type st_1 from statictext within w_info_gral_eva_calif_y_resol
end type
type em_fechadesde from editmask within w_info_gral_eva_calif_y_resol
end type
type st_13 from statictext within w_info_gral_eva_calif_y_resol
end type
type em_fechahasta from editmask within w_info_gral_eva_calif_y_resol
end type
type st_zona from statictext within w_info_gral_eva_calif_y_resol
end type
type dw_zona from datawindow within w_info_gral_eva_calif_y_resol
end type
type cbx_todoplanta from checkbox within w_info_gral_eva_calif_y_resol
end type
type dw_planta from datawindow within w_info_gral_eva_calif_y_resol
end type
type st_33 from statictext within w_info_gral_eva_calif_y_resol
end type
type dw_agronomo from datawindow within w_info_gral_eva_calif_y_resol
end type
type cbx_todoszona from checkbox within w_info_gral_eva_calif_y_resol
end type
type cbx_todosagro from checkbox within w_info_gral_eva_calif_y_resol
end type
type cbx_todosfecha from checkbox within w_info_gral_eva_calif_y_resol
end type
type gb_4 from groupbox within w_info_gral_eva_calif_y_resol
end type
type st_12 from statictext within w_info_gral_eva_calif_y_resol
end type
type st_44 from statictext within w_info_gral_eva_calif_y_resol
end type
type st_2 from statictext within w_info_gral_eva_calif_y_resol
end type
type rb_1 from radiobutton within w_info_gral_eva_calif_y_resol
end type
type rb_2 from radiobutton within w_info_gral_eva_calif_y_resol
end type
type gb_8 from groupbox within w_info_gral_eva_calif_y_resol
end type
type rb_porcentaje from radiobutton within w_info_gral_eva_calif_y_resol
end type
type rb_cajas from radiobutton within w_info_gral_eva_calif_y_resol
end type
type st_5 from statictext within w_info_gral_eva_calif_y_resol
end type
type st_6 from statictext within w_info_gral_eva_calif_y_resol
end type
type st_7 from statictext within w_info_gral_eva_calif_y_resol
end type
type gb_3 from groupbox within w_info_gral_eva_calif_y_resol
end type
type st_4 from statictext within w_info_gral_eva_calif_y_resol
end type
type st_8 from statictext within w_info_gral_eva_calif_y_resol
end type
type st_11 from statictext within w_info_gral_eva_calif_y_resol
end type
type dw_especie from datawindow within w_info_gral_eva_calif_y_resol
end type
end forward

global type w_info_gral_eva_calif_y_resol from w_para_informes
integer x = 14
integer y = 32
integer width = 2880
integer height = 1712
string title = "Calificación y Resolución"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_7 gb_7
st_1 st_1
em_fechadesde em_fechadesde
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
dw_zona dw_zona
cbx_todoplanta cbx_todoplanta
dw_planta dw_planta
st_33 st_33
dw_agronomo dw_agronomo
cbx_todoszona cbx_todoszona
cbx_todosagro cbx_todosagro
cbx_todosfecha cbx_todosfecha
gb_4 gb_4
st_12 st_12
st_44 st_44
st_2 st_2
rb_1 rb_1
rb_2 rb_2
gb_8 gb_8
rb_porcentaje rb_porcentaje
rb_cajas rb_cajas
st_5 st_5
st_6 st_6
st_7 st_7
gb_3 gb_3
st_4 st_4
st_8 st_8
st_11 st_11
dw_especie dw_especie
end type
global w_info_gral_eva_calif_y_resol w_info_gral_eva_calif_y_resol

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, Codigo
String	is_report, Nombre

DataWindowChild		idwc_zona, idwc_planta, idwc_cliente, idwc_tecnico, idwc_especie
uo_zonas					iuo_zonas
uo_ctlcalagronomos	iuo_ctlcalagronomos
uo_especie				iuo_especie

end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexisteagroproduc (integer ai_agronomo)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_zona

Select Count(*)
Into :li_Contador
From dba.plantadesp
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
	messagebox("Atención","Código de Agrónomo No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE
END IF

end function

on w_info_gral_eva_calif_y_resol.create
int iCurrent
call super::create
this.gb_7=create gb_7
this.st_1=create st_1
this.em_fechadesde=create em_fechadesde
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.dw_zona=create dw_zona
this.cbx_todoplanta=create cbx_todoplanta
this.dw_planta=create dw_planta
this.st_33=create st_33
this.dw_agronomo=create dw_agronomo
this.cbx_todoszona=create cbx_todoszona
this.cbx_todosagro=create cbx_todosagro
this.cbx_todosfecha=create cbx_todosfecha
this.gb_4=create gb_4
this.st_12=create st_12
this.st_44=create st_44
this.st_2=create st_2
this.rb_1=create rb_1
this.rb_2=create rb_2
this.gb_8=create gb_8
this.rb_porcentaje=create rb_porcentaje
this.rb_cajas=create rb_cajas
this.st_5=create st_5
this.st_6=create st_6
this.st_7=create st_7
this.gb_3=create gb_3
this.st_4=create st_4
this.st_8=create st_8
this.st_11=create st_11
this.dw_especie=create dw_especie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_7
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_fechadesde
this.Control[iCurrent+4]=this.st_13
this.Control[iCurrent+5]=this.em_fechahasta
this.Control[iCurrent+6]=this.st_zona
this.Control[iCurrent+7]=this.dw_zona
this.Control[iCurrent+8]=this.cbx_todoplanta
this.Control[iCurrent+9]=this.dw_planta
this.Control[iCurrent+10]=this.st_33
this.Control[iCurrent+11]=this.dw_agronomo
this.Control[iCurrent+12]=this.cbx_todoszona
this.Control[iCurrent+13]=this.cbx_todosagro
this.Control[iCurrent+14]=this.cbx_todosfecha
this.Control[iCurrent+15]=this.gb_4
this.Control[iCurrent+16]=this.st_12
this.Control[iCurrent+17]=this.st_44
this.Control[iCurrent+18]=this.st_2
this.Control[iCurrent+19]=this.rb_1
this.Control[iCurrent+20]=this.rb_2
this.Control[iCurrent+21]=this.gb_8
this.Control[iCurrent+22]=this.rb_porcentaje
this.Control[iCurrent+23]=this.rb_cajas
this.Control[iCurrent+24]=this.st_5
this.Control[iCurrent+25]=this.st_6
this.Control[iCurrent+26]=this.st_7
this.Control[iCurrent+27]=this.gb_3
this.Control[iCurrent+28]=this.st_4
this.Control[iCurrent+29]=this.st_8
this.Control[iCurrent+30]=this.st_11
this.Control[iCurrent+31]=this.dw_especie
end on

on w_info_gral_eva_calif_y_resol.destroy
call super::destroy
destroy(this.gb_7)
destroy(this.st_1)
destroy(this.em_fechadesde)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.dw_zona)
destroy(this.cbx_todoplanta)
destroy(this.dw_planta)
destroy(this.st_33)
destroy(this.dw_agronomo)
destroy(this.cbx_todoszona)
destroy(this.cbx_todosagro)
destroy(this.cbx_todosfecha)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.st_44)
destroy(this.st_2)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.gb_8)
destroy(this.rb_porcentaje)
destroy(this.rb_cajas)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_8)
destroy(this.st_11)
destroy(this.dw_especie)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

iuo_zonas				 =  Create uo_zonas
iuo_ctlcalagronomos	 =  Create uo_ctlcalagronomos
iuo_especie  =	Create uo_especie

//zona
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
idwc_zona.SetSort("Zona_nombre asc")
idwc_zona.Sort()

//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1,0)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_planta.InsertRow(0)
//Especie
dw_Especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
idwc_especie.SetSort("espe_nombre A")
idwc_especie.Sort()
dw_Especie.InsertRow(0)

//Técnicos
dw_agronomo.GetChild("ccag_codigo", idwc_tecnico)
idwc_tecnico.SetTransObject(sqlca)
idwc_tecnico.Retrieve(0)
dw_agronomo.InsertRow(0)
idwc_tecnico.SetSort("ccag_nombre asc")

em_fechadesde.text	=	String(Today())
em_fechahasta.text	=	String(Today())

cbx_todoszona.SetFocus()
end event

type st_computador from w_para_informes`st_computador within w_info_gral_eva_calif_y_resol
end type

type st_usuario from w_para_informes`st_usuario within w_info_gral_eva_calif_y_resol
end type

type st_temporada from w_para_informes`st_temporada within w_info_gral_eva_calif_y_resol
end type

type p_logo from w_para_informes`p_logo within w_info_gral_eva_calif_y_resol
end type

type st_titulo from w_para_informes`st_titulo within w_info_gral_eva_calif_y_resol
integer x = 73
integer y = 64
integer width = 2359
string text = "Informe de Calificación y Resolución por Variedad"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_gral_eva_calif_y_resol
string tag = "Imprimir Reporte"
integer x = 2555
integer y = 352
integer taborder = 150
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_planta,li_Agronomo, li_Tipo, li_zona, li_especie
Date		ld_FechaEmbaini, ld_FechaEmbafin

SetPointer(HourGlass!)

IF cbx_todoszona.Checked THEN
	li_zona	= 999
ELSE
	li_zona	= dw_zona.Object.zona_codigo[1]
END IF

IF cbx_todoplanta.Checked THEN
	li_planta	 = 9999
ELSE
	li_planta	 = dw_planta.Object.plde_codigo[1]
END IF

li_especie	= dw_especie.Object.espe_codigo[1]
IF IsNull(li_especie)THEN
	MessageBox("Atención","Debe Seleccionar una especie Previamente",Exclamation!)
	RETURN
END IF

IF cbx_todosagro.Checked THEN
	li_agronomo	 = 9999
ELSE
	li_agronomo	= dw_agronomo.Object.ccag_codigo[1]
	IF IsNull(li_agronomo)THEN
		MessageBox("Atención","Debe Seleccionar un Agronomo Previamente",Exclamation!)
		RETURN
	END IF
END IF	

IF cbx_todosfecha.Checked THEN
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
ELSE
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
END IF

istr_info.titulo	= 'INFORME CALIFICACION Y RESOLUCION POR VARIEDAD'

OpenWithParm(vinf,istr_info)

IF rb_1.Checked AND iuo_especie.codigo = 11 THEN
	vinf.dw_1.DataObject = "dw_info_general_eval_calif_resol"//variedad
ELSEIF rb_2.Checked AND iuo_especie.codigo = 11 THEN
	vinf.dw_1.DataObject = "dw_info_historico_eval_calif_resol"//prod.y variedad
ELSEIF rb_1.Checked AND iuo_especie.codigo <> 11 THEN
	vinf.dw_1.DataObject = "dw_informe_general_eval_calif_resol"//variedad
//ELSE 
//	vinf.dw_1.DataObject = "dw_informe_historico_eval_calif_resol"//prod.y variedad
END IF
	
IF rb_Porcentaje.Checked THEN
	li_Tipo = 1
ELSE
	li_Tipo = 0
END IF

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_codexport,li_zona, li_agronomo, li_Planta, ld_FechaEmbaini, &
									  ld_FechaEmbafin,li_especie,li_Tipo)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_fechahasta.text + "'")
	
	IF rb_1.Checked THEN
		
		IF li_agronomo	 = 9999 THEN
			vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
		END IF
		
		IF cbx_todoszona.Checked THEN
			vinf.dw_1.Modify("Codigo.text = '" + String('999','000') + "'")
			vinf.dw_1.Modify("nombre.text = '" + 'Todas' + "'")
		ELSE
			vinf.dw_1.Modify("Codigo.text = '" + String(iuo_zonas.Codigo,'000') + "'")
			vinf.dw_1.Modify("nombre.text = '" + iuo_zonas.Nombre + "'")
		END IF
	END IF
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_gral_eva_calif_y_resol
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2551
integer y = 632
integer taborder = 160
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_7 from groupbox within w_info_gral_eva_calif_y_resol
integer x = 110
integer y = 244
integer width = 2258
integer height = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_1 from statictext within w_info_gral_eva_calif_y_resol
integer x = 178
integer y = 692
integer width = 311
integer height = 84
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

type em_fechadesde from editmask within w_info_gral_eva_calif_y_resol
integer x = 832
integer y = 1200
integer width = 370
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

type st_13 from statictext within w_info_gral_eva_calif_y_resol
integer x = 1477
integer y = 1144
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

type em_fechahasta from editmask within w_info_gral_eva_calif_y_resol
integer x = 1472
integer y = 1200
integer width = 370
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

type st_zona from statictext within w_info_gral_eva_calif_y_resol
integer x = 178
integer y = 572
integer width = 462
integer height = 84
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

type dw_zona from datawindow within w_info_gral_eva_calif_y_resol
integer x = 704
integer y = 568
integer width = 841
integer height = 92
integer taborder = 40
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
	
	dw_agronomo.GetChild("ccag_codigo", idwc_tecnico)
	idwc_tecnico.SetTransObject(sqlca)
	idwc_tecnico.Retrieve(Integer(Data))
	dw_agronomo.SetItem(1, "ccag_codigo", Long(ls_Nula))
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1,Integer(data))
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	dw_planta.InsertRow(0)
	
	RETURN 0
END IF	

end event

event itemerror;Return 1
end event

event clicked;cbx_todoszona.Checked = False
end event

type cbx_todoplanta from checkbox within w_info_gral_eva_calif_y_resol
integer x = 2011
integer y = 700
integer width = 142
integer height = 64
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
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	//dw_planta.Enabled = FALSE	
   dw_planta.SetItem(1, "plde_codigo", li_null)
ELSE
	//dw_planta.Enabled 		=  TRUE
	dw_planta.Setfocus()
	dw_planta.SetItem(1, "plde_codigo", li_null)
	
END IF
RETURN 0

end event

type dw_planta from datawindow within w_info_gral_eva_calif_y_resol
integer x = 704
integer y = 688
integer width = 969
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
end event

type st_33 from statictext within w_info_gral_eva_calif_y_resol
integer x = 178
integer y = 932
integer width = 315
integer height = 84
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

type dw_agronomo from datawindow within w_info_gral_eva_calif_y_resol
integer x = 704
integer y = 928
integer width = 1024
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_ctlcalagronomos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Nula
Integer	li_zona

dw_agronomo.PostEvent(Clicked!)

SetNull(ls_Nula)

IF NoExisteagroproduc(Integer(data)) THEN
    This.SetItem(1,"ccag_codigo", Long(ls_nula))
    RETURN 1
END IF	

end event

event itemerror;RETURN 1
end event

event clicked;cbx_todosagro.Checked = False
end event

type cbx_todoszona from checkbox within w_info_gral_eva_calif_y_resol
integer x = 2011
integer y = 580
integer width = 142
integer height = 64
integer taborder = 30
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

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	//dw_zona.Enabled = False	
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(gi_codexport,1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
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

type cbx_todosagro from checkbox within w_info_gral_eva_calif_y_resol
integer x = 2011
integer y = 952
integer width = 142
integer height = 64
integer taborder = 80
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
	//dw_agronomo.Enabled	=	FALSE	
	dw_agronomo.SetItem(1, "ccag_codigo", li_null)
ELSE
	//dw_agronomo.Enabled	=	TRUE
	dw_agronomo.Setfocus()
END IF
end event

type cbx_todosfecha from checkbox within w_info_gral_eva_calif_y_resol
integer x = 2011
integer y = 1212
integer width = 142
integer height = 64
integer taborder = 100
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

type gb_4 from groupbox within w_info_gral_eva_calif_y_resol
integer x = 110
integer y = 456
integer width = 2258
integer height = 600
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type st_12 from statictext within w_info_gral_eva_calif_y_resol
integer x = 841
integer y = 1144
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

type st_44 from statictext within w_info_gral_eva_calif_y_resol
integer x = 73
integer y = 448
integer width = 2359
integer height = 640
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

type st_2 from statictext within w_info_gral_eva_calif_y_resol
integer x = 73
integer y = 232
integer width = 2359
integer height = 216
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

type rb_1 from radiobutton within w_info_gral_eva_calif_y_resol
integer x = 558
integer y = 304
integer width = 617
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Por Variedad"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_gral_eva_calif_y_resol
integer x = 1458
integer y = 304
integer width = 864
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Por Productor y Variedad"
end type

type gb_8 from groupbox within w_info_gral_eva_calif_y_resol
integer x = 151
integer y = 1312
integer width = 2176
integer height = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type rb_porcentaje from radiobutton within w_info_gral_eva_calif_y_resol
integer x = 658
integer y = 1368
integer width = 745
integer height = 84
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Porcentaje"
boolean checked = true
end type

type rb_cajas from radiobutton within w_info_gral_eva_calif_y_resol
integer x = 1678
integer y = 1368
integer width = 530
integer height = 80
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cajas"
end type

type st_5 from statictext within w_info_gral_eva_calif_y_resol
integer x = 178
integer y = 304
integer width = 256
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Informe:"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_gral_eva_calif_y_resol
integer x = 169
integer y = 1372
integer width = 384
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tipo Informe"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_gral_eva_calif_y_resol
integer x = 224
integer y = 1208
integer width = 485
integer height = 76
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
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_gral_eva_calif_y_resol
integer x = 110
integer y = 1100
integer width = 2258
integer height = 428
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type st_4 from statictext within w_info_gral_eva_calif_y_resol
integer x = 69
integer y = 1088
integer width = 2359
integer height = 480
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

type st_8 from statictext within w_info_gral_eva_calif_y_resol
integer x = 1966
integer y = 508
integer width = 283
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_gral_eva_calif_y_resol
integer x = 178
integer y = 812
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_gral_eva_calif_y_resol
integer x = 704
integer y = 808
integer width = 859
integer height = 92
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)

IF iuo_Especie.Existe(Integer(data), True, sqlca) =False THEN
		This.SetItem(1, "espe_codigo", long(li_Nula))
		RETURN 1
	ELSEIF iuo_Especie.codigo <>11 THEN
		rb_2.Enabled	=	False
		rb_1.Checked	=	True
	ELSE
		rb_2.Enabled	=	True
END IF


end event

event itemerror;Return 1
end event

