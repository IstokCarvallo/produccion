$PBExportHeader$w_info_ctlcalpesopromedio.srw
$PBExportComments$Ventana de Informe de Peso Promedio por Racimo
forward
global type w_info_ctlcalpesopromedio from w_para_informes
end type
type gb_7 from groupbox within w_info_ctlcalpesopromedio
end type
type em_desde from editmask within w_info_ctlcalpesopromedio
end type
type em_hasta from editmask within w_info_ctlcalpesopromedio
end type
type st_5 from statictext within w_info_ctlcalpesopromedio
end type
type st_6 from statictext within w_info_ctlcalpesopromedio
end type
type dw_zona from datawindow within w_info_ctlcalpesopromedio
end type
type dw_prod from datawindow within w_info_ctlcalpesopromedio
end type
type dw_variedades from datawindow within w_info_ctlcalpesopromedio
end type
type cbx_todosvar from checkbox within w_info_ctlcalpesopromedio
end type
type cbx_consvariedad from checkbox within w_info_ctlcalpesopromedio
end type
type cbx_todoprod from checkbox within w_info_ctlcalpesopromedio
end type
type cbx_consprod from checkbox within w_info_ctlcalpesopromedio
end type
type cbx_todoszona from checkbox within w_info_ctlcalpesopromedio
end type
type cbx_conzona from checkbox within w_info_ctlcalpesopromedio
end type
type gb_4 from groupbox within w_info_ctlcalpesopromedio
end type
type gb_3 from groupbox within w_info_ctlcalpesopromedio
end type
type st_44 from statictext within w_info_ctlcalpesopromedio
end type
type dw_2 from datawindow within w_info_ctlcalpesopromedio
end type
type cbx_confecha from checkbox within w_info_ctlcalpesopromedio
end type
type st_1 from statictext within w_info_ctlcalpesopromedio
end type
type st_2 from statictext within w_info_ctlcalpesopromedio
end type
type st_3 from statictext within w_info_ctlcalpesopromedio
end type
type st_4 from statictext within w_info_ctlcalpesopromedio
end type
type st_7 from statictext within w_info_ctlcalpesopromedio
end type
type st_8 from statictext within w_info_ctlcalpesopromedio
end type
type st_9 from statictext within w_info_ctlcalpesopromedio
end type
type cbx_embalaje from checkbox within w_info_ctlcalpesopromedio
end type
type dw_emba from datawindow within w_info_ctlcalpesopromedio
end type
type cbx_4 from checkbox within w_info_ctlcalpesopromedio
end type
end forward

global type w_info_ctlcalpesopromedio from w_para_informes
integer x = 14
integer y = 32
integer width = 3159
integer height = 1416
string title = "Peso Promedio de Racimo"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_7 gb_7
em_desde em_desde
em_hasta em_hasta
st_5 st_5
st_6 st_6
dw_zona dw_zona
dw_prod dw_prod
dw_variedades dw_variedades
cbx_todosvar cbx_todosvar
cbx_consvariedad cbx_consvariedad
cbx_todoprod cbx_todoprod
cbx_consprod cbx_consprod
cbx_todoszona cbx_todoszona
cbx_conzona cbx_conzona
gb_4 gb_4
gb_3 gb_3
st_44 st_44
dw_2 dw_2
cbx_confecha cbx_confecha
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
st_7 st_7
st_8 st_8
st_9 st_9
cbx_embalaje cbx_embalaje
dw_emba dw_emba
cbx_4 cbx_4
end type
global w_info_ctlcalpesopromedio w_info_ctlcalpesopromedio

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report, is_porc

DataWindowChild		idwc_zona, idwc_productores,idwc_planta, idwc_cliente,&
							idwc_variedad, idwc_tecnico, idwc_inspector, idwc_packing,idwc_prod, idwc_emba



////////////////////////////////


//DataWindowChild		idwc_zona, idwc_planta, idwc_cliente, idwc_tecnico,idwc_especie,idwc_prod,idwc_emba
uo_zonas					iuo_zonas
uo_ctlcaltecnicos		iuo_ctlcaltecnicos
uo_especie				iuo_especie
uo_productores       iuo_productores
//uo_embalajes         iuo_embalajes
uo_embalajesprod     iuo_embalajes


end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexisteproductor (integer al_productor)
public function boolean noexistevariedad (integer variedad)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_cliente,li_zona


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

public function boolean noexisteproductor (integer al_productor);Integer li_Contador, li_zona

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.productores
	WHERE	prod_codigo =	:al_productor
	AND	:li_zona in (0,zona_codigo);

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

public function boolean noexistevariedad (integer variedad);Integer li_Contador

Select Count(*)
Into :li_Contador
From dba.variedades
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

on w_info_ctlcalpesopromedio.create
int iCurrent
call super::create
this.gb_7=create gb_7
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_5=create st_5
this.st_6=create st_6
this.dw_zona=create dw_zona
this.dw_prod=create dw_prod
this.dw_variedades=create dw_variedades
this.cbx_todosvar=create cbx_todosvar
this.cbx_consvariedad=create cbx_consvariedad
this.cbx_todoprod=create cbx_todoprod
this.cbx_consprod=create cbx_consprod
this.cbx_todoszona=create cbx_todoszona
this.cbx_conzona=create cbx_conzona
this.gb_4=create gb_4
this.gb_3=create gb_3
this.st_44=create st_44
this.dw_2=create dw_2
this.cbx_confecha=create cbx_confecha
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.st_7=create st_7
this.st_8=create st_8
this.st_9=create st_9
this.cbx_embalaje=create cbx_embalaje
this.dw_emba=create dw_emba
this.cbx_4=create cbx_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_7
this.Control[iCurrent+2]=this.em_desde
this.Control[iCurrent+3]=this.em_hasta
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_zona
this.Control[iCurrent+7]=this.dw_prod
this.Control[iCurrent+8]=this.dw_variedades
this.Control[iCurrent+9]=this.cbx_todosvar
this.Control[iCurrent+10]=this.cbx_consvariedad
this.Control[iCurrent+11]=this.cbx_todoprod
this.Control[iCurrent+12]=this.cbx_consprod
this.Control[iCurrent+13]=this.cbx_todoszona
this.Control[iCurrent+14]=this.cbx_conzona
this.Control[iCurrent+15]=this.gb_4
this.Control[iCurrent+16]=this.gb_3
this.Control[iCurrent+17]=this.st_44
this.Control[iCurrent+18]=this.dw_2
this.Control[iCurrent+19]=this.cbx_confecha
this.Control[iCurrent+20]=this.st_1
this.Control[iCurrent+21]=this.st_2
this.Control[iCurrent+22]=this.st_3
this.Control[iCurrent+23]=this.st_4
this.Control[iCurrent+24]=this.st_7
this.Control[iCurrent+25]=this.st_8
this.Control[iCurrent+26]=this.st_9
this.Control[iCurrent+27]=this.cbx_embalaje
this.Control[iCurrent+28]=this.dw_emba
this.Control[iCurrent+29]=this.cbx_4
end on

on w_info_ctlcalpesopromedio.destroy
call super::destroy
destroy(this.gb_7)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.dw_zona)
destroy(this.dw_prod)
destroy(this.dw_variedades)
destroy(this.cbx_todosvar)
destroy(this.cbx_consvariedad)
destroy(this.cbx_todoprod)
destroy(this.cbx_consprod)
destroy(this.cbx_todoszona)
destroy(this.cbx_conzona)
destroy(this.gb_4)
destroy(this.gb_3)
destroy(this.st_44)
destroy(this.dw_2)
destroy(this.cbx_confecha)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.cbx_embalaje)
destroy(this.dw_emba)
destroy(this.cbx_4)
end on

event open;Boolean	lb_Cerrar
String	ls_filtro
x = 0
y = 0

iuo_zonas		=  Create uo_zonas
iuo_embalajes	=  Create   uo_embalajesprod

//Zona
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)

//Productor
dw_prod.GetChild("prod_codigo", idwc_prod)
idwc_prod.SetTransObject(sqlca)
idwc_prod.Retrieve()
dw_prod.InsertRow(0)
idwc_prod.SetSort("prod_nombre asc")
idwc_prod.Sort()

//Variedades
dw_variedades.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(11)
dw_variedades.InsertRow(0)
idwc_variedad.SetSort("Vari_nombre asc")
idwc_variedad.Sort()

//Embalajes
dw_emba.GetChild("emba_codigo", idwc_emba)
idwc_emba.SetTransObject(sqlca)
idwc_emba.Retrieve(gi_codexport)
dw_emba.InsertRow(0)

ls_Filtro = "Mid(emba_codigo,1,1) = 'U'" 
idwc_emba.SetFilter(ls_Filtro)
idwc_emba.Filter()

This.Icon	=	Gstr_apl.Icono
end event

type st_computador from w_para_informes`st_computador within w_info_ctlcalpesopromedio
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcalpesopromedio
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcalpesopromedio
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcalpesopromedio
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcalpesopromedio
integer x = 69
integer y = 64
integer width = 2642
string text = "Informe de Peso Promedio de Racimo"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcalpesopromedio
string tag = "Imprimir Reporte"
integer x = 2857
integer y = 336
integer taborder = 130
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_planta,li_Variedad, li_Agronomo,li_Packing, &
			li_zona, li_ConsPlanta, li_ConsPacking, li_conszonas, li_ConsAgro,li_filas,&
			li_ConsFechaEmb, li_ConsCalibre, li_ConsEmbalaje, li_ConsProd,li_ConsVari
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_calibre, ls_embalaje, ls_desde, ls_hasta, ls_emba
Long     ll_Productor 

SetPointer(HourGlass!)

istr_info.titulo	= 'PESO PROMEDIO POR RACIMO'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_racimo"
vinf.dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

IF cbx_todoszona.Checked THEN
	li_zona	= -1
ELSEIF cbx_conzona.Checked THEN
	li_zona=-9
ELSE
	li_zona	= dw_zona.Object.zona_codigo[1]
END IF

IF cbx_todoprod.Checked THEN
	ll_productor	 = -1
ELSE
	IF cbx_consprod.Checked THEN
		ll_productor = -9
   ELSE
		ll_productor	= dw_prod.Object.prod_codigo[1]
			IF IsNull(ll_productor)THEN
				MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
				RETURN
			END IF
	END IF
END IF	

IF cbx_todosvar.Checked THEN
	li_variedad	 = -1
ELSE
	IF cbx_consvariedad.Checked THEN
		li_variedad = -9
	ELSE
		li_variedad  = dw_variedades.Object.vari_codigo[1]
	END IF
END IF	

IF cbx_confecha.Checked THEN
	ls_desde				=	"18000101"
	ls_hasta				=	"18000101"
	em_desde.enabled	=	TRUE
	em_desde.text		=	"01/01/1800"
	em_hasta.enabled	=	TRUE
	em_hasta.text		=	"01/01/1800"
	ld_FechaEmbaini 	= 	Date(em_desde.text)
   ld_FechaEmbafin 	= 	Date(em_hasta.text)
	em_desde.enabled	=	FALSE
	em_hasta.enabled	=	FALSE
ELSE
ld_FechaEmbaini = Date(em_desde.Text)
ld_FechaEmbafin = Date(em_hasta.Text)

ls_desde	=	em_desde.Text
ls_hasta	=	em_hasta.Text
END IF

//Embalaje
IF cbx_4.Checked THEN
	ls_emba = '**'
ELSE
   ls_emba	= dw_emba.Object.emba_codigo[1]
	IF IsNull(ls_emba) THEN
	  MessageBox("Atención","Debe Seleccionar un Embalaje Previamente",Exclamation!)
	  RETURN
	END IF
END IF

li_filas= dw_2.Retrieve(gi_codexport, li_zona, ll_productor,&
						  11, li_variedad,ld_FechaEmbaini,ld_FechaEmbafin,ls_emba)

IF ls_desde =	"" OR ls_hasta	=	"" OR ls_desde	=	"00/00/0000" OR ls_hasta	=	"00/00/0000"  THEN
	ls_desde	=	"00/00/0000"
	MessageBox("Verifique Fecha",ls_desde) 
ELSE
	
	li_fila = vinf.dw_1.Retrieve(gi_codexport, li_zona, ll_productor,&
							  11, li_variedad,ld_FechaEmbaini,ld_FechaEmbafin,ls_emba)

	IF li_filas = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF li_filas = 0 THEN
		MessageBox( "No Existe información", "No Existe información para este informe", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		IF cbx_confecha.checked	=	True THEN
			vinf.dw_1.Modify("t_desde.text = '" + "Consolidado" + "'")
			vinf.dw_1.Modify("t_hasta.text = '" + "Consolidado" + "'")
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		
			END IF	
		ELSE		
			vinf.dw_1.Modify("t_desde.text = '" + em_desde.Text + "'")
			vinf.dw_1.Modify("t_hasta.text = '" + em_hasta.text + "'")
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		
			END IF
		END IF	
	END IF
END IF
SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcalpesopromedio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2857
integer y = 624
integer taborder = 140
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_7 from groupbox within w_info_ctlcalpesopromedio
integer x = 229
integer y = 932
integer width = 2318
integer height = 232
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type em_desde from editmask within w_info_ctlcalpesopromedio
integer x = 910
integer y = 1028
integer width = 366
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_hasta from editmask within w_info_ctlcalpesopromedio
integer x = 1582
integer y = 1036
integer width = 352
integer height = 88
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_5 from statictext within w_info_ctlcalpesopromedio
integer x = 910
integer y = 976
integer width = 279
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_ctlcalpesopromedio
integer x = 1577
integer y = 976
integer width = 215
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
string text = "Hasta"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_ctlcalpesopromedio
integer x = 791
integer y = 340
integer width = 864
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;Return 1
end event

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_zona.PostEvent(Clicked!)

IF iuo_zonas.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "zona_codigo", Long(ls_nula))
	RETURN 1
ELSE
	RETURN 0
END IF	



end event

event clicked;cbx_todoszona.checked = False
cbx_conzona.checked = False
end event

type dw_prod from datawindow within w_info_ctlcalpesopromedio
integer x = 791
integer y = 480
integer width = 960
integer height = 96
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;Return 1
end event

event itemchanged;String ls_Nula

dw_prod.PostEvent(Clicked!)

SetNull(ls_Nula)
 IF NoExisteProductor(Long(data)) THEN
    This.SetItem(1,"prod_codigo", Long(ls_nula))
    RETURN 1

	
END IF	
end event

event clicked;cbx_todoprod.checked = False
cbx_consprod.checked = False
end event

type dw_variedades from datawindow within w_info_ctlcalpesopromedio
integer x = 791
integer y = 624
integer width = 869
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;Return 1
end event

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_variedades.PostEvent(Clicked!)

IF NoExisteVariedad(Integer(data)) THEN
  THIS.SetItem(1, "vari_codigo", Long(ls_nula))
  RETURN 1
ELSE
  RETURN 0
END IF  
end event

event clicked;cbx_todosvar.checked = False
cbx_consvariedad.checked = False
end event

type cbx_todosvar from checkbox within w_info_ctlcalpesopromedio
integer x = 1870
integer y = 628
integer width = 123
integer height = 80
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
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	cbx_consvariedad.checked=FALSE
	//dw_variedades.Enabled = FALSE	
   dw_variedades.SetItem(1, "vari_codigo", li_null)
ELSE
	IF	cbx_consvariedad.checked=TRUE THEN
	ELSE	
	//dw_variedades.Enabled 		=  TRUE
	dw_variedades.SetFocus()

	dw_variedades.SetItem(1, "vari_codigo", li_null)
	END IF
END IF
RETURN 0

end event

type cbx_consvariedad from checkbox within w_info_ctlcalpesopromedio
integer x = 2235
integer y = 628
integer width = 123
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	cbx_todosvar.checked=FALSE
	//dw_variedades.Enabled = FALSE	
   dw_variedades.SetItem(1, "vari_codigo", li_null)
ELSE
	IF	cbx_todosvar.checked=TRUE THEN
	ELSE	
	//dw_variedades.Enabled 		=  TRUE
	dw_variedades.Setfocus()
	dw_variedades.SetItem(1, "vari_codigo", li_null)
	END IF
END IF
RETURN 0

end event

type cbx_todoprod from checkbox within w_info_ctlcalpesopromedio
integer x = 1870
integer y = 488
integer width = 123
integer height = 80
integer taborder = 50
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

SetNull(li_Null)

IF This.Checked THEN
	cbx_consprod.checked=FALSE
	//dw_prod.Enabled = FALSE	
   dw_prod.SetItem(1, "prod_codigo", li_null)
ELSE
	IF	cbx_consprod.checked=TRUE THEN
	ELSE	
	//dw_Prod.Enabled 		=  TRUE
	dw_prod.SetFocus()
	dw_Prod.Setfocus()
	//dw_planta.SetItem(1, "plde_codigo", )
	END IF
END IF
RETURN 0

end event

type cbx_consprod from checkbox within w_info_ctlcalpesopromedio
integer x = 2240
integer y = 488
integer width = 123
integer height = 80
integer taborder = 40
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

SetNull(li_Null)

IF This.Checked THEN
	cbx_todoprod.checked=FALSE
	//dw_prod.Enabled = FALSE	
   dw_prod.SetItem(1, "prod_codigo", li_null)
ELSE
	IF	cbx_todoprod.checked=TRUE THEN
	ELSE	
	//dw_prod.Enabled 		=  TRUE
	dw_prod.Setfocus()
	
	END IF
END IF
RETURN 0

end event

type cbx_todoszona from checkbox within w_info_ctlcalpesopromedio
integer x = 1870
integer y = 344
integer width = 123
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
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_null)
	

IF This.Checked THEN
	CBX_conZONA.CHECKED=FALSE
	//dw_zona.Enabled = FALSE	
	dw_zona.SetItem(1, "zona_codigo", li_null)
		
ELSE
		IF	cbx_conzona.CHECKED= TRUE THEN
		   
		  // CBX_TODOSZONA.CHECKED=FALSE
			
		ELSE
			//dw_zona.Enabled = True
			dw_zona.SetFocus()
			dw_zona.SetItem(1, "zona_codigo", li_null)
			
		END IF	
END IF

RETURN 0

end event

type cbx_conzona from checkbox within w_info_ctlcalpesopromedio
integer x = 2240
integer y = 344
integer width = 123
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
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_null)
	

IF This.Checked THEN
	CBX_todosZONA.CHECKED=FALSE
	//dw_zona.Enabled = FALSE	
	dw_zona.SetItem(1, "zona_codigo", li_null)
	
	
ELSE
		IF	cbx_todoszona.CHECKED= TRUE THEN
		   
		  // CBX_TODOSZONA.CHECKED=FALSE
			
		ELSE
			//dw_zona.Enabled = True
			dw_zona.SetFocus()
			dw_zona.SetItem(1, "zona_codigo", li_null)
			
		END IF	
END IF

RETURN 0
end event

type gb_4 from groupbox within w_info_ctlcalpesopromedio
integer x = 229
integer y = 240
integer width = 2318
integer height = 680
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_3 from groupbox within w_info_ctlcalpesopromedio
integer x = 146
integer y = 196
integer width = 2487
integer height = 1008
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_44 from statictext within w_info_ctlcalpesopromedio
integer x = 69
integer y = 184
integer width = 2647
integer height = 1076
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

type dw_2 from datawindow within w_info_ctlcalpesopromedio
boolean visible = false
integer x = 2798
integer y = 876
integer width = 261
integer height = 384
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_racimopeso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cbx_confecha from checkbox within w_info_ctlcalpesopromedio
integer x = 2240
integer y = 1040
integer width = 101
integer height = 80
integer taborder = 100
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

SetNull(li_Null)

IF This.Checked THEN
	em_desde.enabled=FALSE
	em_hasta.enabled=FALSE
ELSE
	em_desde.enabled=TRUE
	em_hasta.enabled=TRUE
	em_desde.SetFocus()
END IF
RETURN 0

end event

type st_1 from statictext within w_info_ctlcalpesopromedio
integer x = 1810
integer y = 284
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_ctlcalpesopromedio
integer x = 2089
integer y = 284
integer width = 389
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidado"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_ctlcalpesopromedio
integer x = 279
integer y = 352
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_ctlcalpesopromedio
integer x = 279
integer y = 500
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_ctlcalpesopromedio
integer x = 279
integer y = 636
integer width = 311
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_ctlcalpesopromedio
integer x = 279
integer y = 1036
integer width = 485
integer height = 100
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

type st_9 from statictext within w_info_ctlcalpesopromedio
integer x = 279
integer y = 792
integer width = 311
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_ctlcalpesopromedio
integer x = 2240
integer y = 768
integer width = 123
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	cbx_todosvar.checked=FALSE
	//dw_variedades.Enabled = FALSE	
   dw_variedades.SetItem(1, "vari_codigo", li_null)
ELSE
	IF	cbx_todosvar.checked=TRUE THEN
	ELSE	
	//dw_variedades.Enabled 		=  TRUE
	dw_variedades.Setfocus()
	dw_variedades.SetItem(1, "vari_codigo", li_null)
	END IF
END IF
RETURN 0

end event

type dw_emba from datawindow within w_info_ctlcalpesopromedio
integer x = 791
integer y = 776
integer width = 969
integer height = 92
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_embalajesprod"
boolean border = false
boolean livescroll = true
end type

event clicked;//cbx_todoemba.Checked = False
cbx_4.Checked = False
end event

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

type cbx_4 from checkbox within w_info_ctlcalpesopromedio
integer x = 2235
integer y = 772
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
	dw_emba.SetItem(1, "emba_codigo", ls_null)
ELSE
	//dw_emba.Enabled 	=  TRUE
	dw_emba.Setfocus()
	dw_emba.SetItem(1, "emba_codigo", ls_null)
	
END IF
RETURN 0

end event

