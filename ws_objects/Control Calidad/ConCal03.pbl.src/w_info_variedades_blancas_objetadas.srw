$PBExportHeader$w_info_variedades_blancas_objetadas.srw
$PBExportComments$Ventana de Informe General de Revisión Inspección y Resolución
forward
global type w_info_variedades_blancas_objetadas from w_para_informes
end type
type st_1 from statictext within w_info_variedades_blancas_objetadas
end type
type st_13 from statictext within w_info_variedades_blancas_objetadas
end type
type em_fechahasta from editmask within w_info_variedades_blancas_objetadas
end type
type st_zona from statictext within w_info_variedades_blancas_objetadas
end type
type dw_zona from datawindow within w_info_variedades_blancas_objetadas
end type
type cbx_todoplanta from checkbox within w_info_variedades_blancas_objetadas
end type
type dw_planta from datawindow within w_info_variedades_blancas_objetadas
end type
type st_33 from statictext within w_info_variedades_blancas_objetadas
end type
type dw_tecnico from datawindow within w_info_variedades_blancas_objetadas
end type
type cbx_todoszona from checkbox within w_info_variedades_blancas_objetadas
end type
type cbx_todosagro from checkbox within w_info_variedades_blancas_objetadas
end type
type cbx_todosfecha from checkbox within w_info_variedades_blancas_objetadas
end type
type st_12 from statictext within w_info_variedades_blancas_objetadas
end type
type dw_especie from datawindow within w_info_variedades_blancas_objetadas
end type
type cbx_espe from checkbox within w_info_variedades_blancas_objetadas
end type
type st_3 from statictext within w_info_variedades_blancas_objetadas
end type
type st_5 from statictext within w_info_variedades_blancas_objetadas
end type
type cbx_todoproduc from checkbox within w_info_variedades_blancas_objetadas
end type
type dw_prod from datawindow within w_info_variedades_blancas_objetadas
end type
type st_6 from statictext within w_info_variedades_blancas_objetadas
end type
type cbx_todosdefe from checkbox within w_info_variedades_blancas_objetadas
end type
type ddlb_filtro from dropdownlistbox within w_info_variedades_blancas_objetadas
end type
type cbx_todoemba from checkbox within w_info_variedades_blancas_objetadas
end type
type dw_emba from datawindow within w_info_variedades_blancas_objetadas
end type
type st_8 from statictext within w_info_variedades_blancas_objetadas
end type
type st_4 from statictext within w_info_variedades_blancas_objetadas
end type
type st_2 from statictext within w_info_variedades_blancas_objetadas
end type
type st_7 from statictext within w_info_variedades_blancas_objetadas
end type
type st_9 from statictext within w_info_variedades_blancas_objetadas
end type
type dw_variedad from datawindow within w_info_variedades_blancas_objetadas
end type
type consfrigo from checkbox within w_info_variedades_blancas_objetadas
end type
type consagro from checkbox within w_info_variedades_blancas_objetadas
end type
type cbx_todovariedad from checkbox within w_info_variedades_blancas_objetadas
end type
type rb_consol from radiobutton within w_info_variedades_blancas_objetadas
end type
type rb_deta from radiobutton within w_info_variedades_blancas_objetadas
end type
type em_fechadesde from editmask within w_info_variedades_blancas_objetadas
end type
type st_10 from statictext within w_info_variedades_blancas_objetadas
end type
type gb_4 from groupbox within w_info_variedades_blancas_objetadas
end type
type conszona from checkbox within w_info_variedades_blancas_objetadas
end type
type consprod from checkbox within w_info_variedades_blancas_objetadas
end type
type consvar from checkbox within w_info_variedades_blancas_objetadas
end type
type consemba from checkbox within w_info_variedades_blancas_objetadas
end type
type cbx_todosvari from checkbox within w_info_variedades_blancas_objetadas
end type
type st_11 from statictext within w_info_variedades_blancas_objetadas
end type
type dw_calibres from datawindow within w_info_variedades_blancas_objetadas
end type
type dw_daños from datawindow within w_info_variedades_blancas_objetadas
end type
type cbx_todoscali from checkbox within w_info_variedades_blancas_objetadas
end type
type sle_1 from singlelineedit within w_info_variedades_blancas_objetadas
end type
end forward

global type w_info_variedades_blancas_objetadas from w_para_informes
integer x = 14
integer y = 32
integer width = 2802
integer height = 1952
string title = "Informe De Variedades Blancas Objetadas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
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
cbx_espe cbx_espe
st_3 st_3
st_5 st_5
cbx_todoproduc cbx_todoproduc
dw_prod dw_prod
st_6 st_6
cbx_todosdefe cbx_todosdefe
ddlb_filtro ddlb_filtro
cbx_todoemba cbx_todoemba
dw_emba dw_emba
st_8 st_8
st_4 st_4
st_2 st_2
st_7 st_7
st_9 st_9
dw_variedad dw_variedad
consfrigo consfrigo
consagro consagro
cbx_todovariedad cbx_todovariedad
rb_consol rb_consol
rb_deta rb_deta
em_fechadesde em_fechadesde
st_10 st_10
gb_4 gb_4
conszona conszona
consprod consprod
consvar consvar
consemba consemba
cbx_todosvari cbx_todosvari
st_11 st_11
dw_calibres dw_calibres
dw_daños dw_daños
cbx_todoscali cbx_todoscali
sle_1 sle_1
end type
global w_info_variedades_blancas_objetadas w_info_variedades_blancas_objetadas

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo,codigo
String	is_report, nombre

DataWindowChild		idwc_zona, idwc_planta, idwc_cliente, idwc_tecnico, &
							idwc_especie, idwc_prod, idwc_emba, idwc_variedad, idwc_defecto
uo_zonas					iuo_zonas
uo_ctlcaltecnicos		iuo_ctlcaltecnicos
uo_especie				iuo_especie
uo_productores       iuo_productores
uo_embalajesprod     iuo_embalajes
uo_variedades        iuo_variedades
end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexistetecnico (integer ai_tecnico)
public function boolean noexisteagroproduc (integer ai_agronomo)
end prototypes

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

public function boolean noexistetecnico (integer ai_tecnico);Integer li_Contador, li_zona, li_tecnico

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
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Agronomos")
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
	FROM	dbo.ctlcalagroproduc
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

on w_info_variedades_blancas_objetadas.create
int iCurrent
call super::create
this.st_1=create st_1
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
this.cbx_espe=create cbx_espe
this.st_3=create st_3
this.st_5=create st_5
this.cbx_todoproduc=create cbx_todoproduc
this.dw_prod=create dw_prod
this.st_6=create st_6
this.cbx_todosdefe=create cbx_todosdefe
this.ddlb_filtro=create ddlb_filtro
this.cbx_todoemba=create cbx_todoemba
this.dw_emba=create dw_emba
this.st_8=create st_8
this.st_4=create st_4
this.st_2=create st_2
this.st_7=create st_7
this.st_9=create st_9
this.dw_variedad=create dw_variedad
this.consfrigo=create consfrigo
this.consagro=create consagro
this.cbx_todovariedad=create cbx_todovariedad
this.rb_consol=create rb_consol
this.rb_deta=create rb_deta
this.em_fechadesde=create em_fechadesde
this.st_10=create st_10
this.gb_4=create gb_4
this.conszona=create conszona
this.consprod=create consprod
this.consvar=create consvar
this.consemba=create consemba
this.cbx_todosvari=create cbx_todosvari
this.st_11=create st_11
this.dw_calibres=create dw_calibres
this.dw_daños=create dw_daños
this.cbx_todoscali=create cbx_todoscali
this.sle_1=create sle_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_13
this.Control[iCurrent+3]=this.em_fechahasta
this.Control[iCurrent+4]=this.st_zona
this.Control[iCurrent+5]=this.dw_zona
this.Control[iCurrent+6]=this.cbx_todoplanta
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_33
this.Control[iCurrent+9]=this.dw_tecnico
this.Control[iCurrent+10]=this.cbx_todoszona
this.Control[iCurrent+11]=this.cbx_todosagro
this.Control[iCurrent+12]=this.cbx_todosfecha
this.Control[iCurrent+13]=this.st_12
this.Control[iCurrent+14]=this.dw_especie
this.Control[iCurrent+15]=this.cbx_espe
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.cbx_todoproduc
this.Control[iCurrent+19]=this.dw_prod
this.Control[iCurrent+20]=this.st_6
this.Control[iCurrent+21]=this.cbx_todosdefe
this.Control[iCurrent+22]=this.ddlb_filtro
this.Control[iCurrent+23]=this.cbx_todoemba
this.Control[iCurrent+24]=this.dw_emba
this.Control[iCurrent+25]=this.st_8
this.Control[iCurrent+26]=this.st_4
this.Control[iCurrent+27]=this.st_2
this.Control[iCurrent+28]=this.st_7
this.Control[iCurrent+29]=this.st_9
this.Control[iCurrent+30]=this.dw_variedad
this.Control[iCurrent+31]=this.consfrigo
this.Control[iCurrent+32]=this.consagro
this.Control[iCurrent+33]=this.cbx_todovariedad
this.Control[iCurrent+34]=this.rb_consol
this.Control[iCurrent+35]=this.rb_deta
this.Control[iCurrent+36]=this.em_fechadesde
this.Control[iCurrent+37]=this.st_10
this.Control[iCurrent+38]=this.gb_4
this.Control[iCurrent+39]=this.conszona
this.Control[iCurrent+40]=this.consprod
this.Control[iCurrent+41]=this.consvar
this.Control[iCurrent+42]=this.consemba
this.Control[iCurrent+43]=this.cbx_todosvari
this.Control[iCurrent+44]=this.st_11
this.Control[iCurrent+45]=this.dw_calibres
this.Control[iCurrent+46]=this.dw_daños
this.Control[iCurrent+47]=this.cbx_todoscali
this.Control[iCurrent+48]=this.sle_1
end on

on w_info_variedades_blancas_objetadas.destroy
call super::destroy
destroy(this.st_1)
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
destroy(this.cbx_espe)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.cbx_todoproduc)
destroy(this.dw_prod)
destroy(this.st_6)
destroy(this.cbx_todosdefe)
destroy(this.ddlb_filtro)
destroy(this.cbx_todoemba)
destroy(this.dw_emba)
destroy(this.st_8)
destroy(this.st_4)
destroy(this.st_2)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.dw_variedad)
destroy(this.consfrigo)
destroy(this.consagro)
destroy(this.cbx_todovariedad)
destroy(this.rb_consol)
destroy(this.rb_deta)
destroy(this.em_fechadesde)
destroy(this.st_10)
destroy(this.gb_4)
destroy(this.conszona)
destroy(this.consprod)
destroy(this.consvar)
destroy(this.consemba)
destroy(this.cbx_todosvari)
destroy(this.st_11)
destroy(this.dw_calibres)
destroy(this.dw_daños)
destroy(this.cbx_todoscali)
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
iuo_variedades       =  Create   uo_variedades

//zona
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
idwc_zona.SetSort("Zona_codigo asc")
idwc_zona.Sort()
//dw_zona.SetItem(1,"zona_codigo", 6)

//Técnicos
dw_tecnico.GetChild("ccag_codigo", idwc_tecnico)
idwc_tecnico.SetTransObject(sqlca)
idwc_tecnico.Retrieve(0)
dw_tecnico.InsertRow(0)
idwc_tecnico.SetSort("ccag_nombre A")
idwc_tecnico.Sort()

//Especie
dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_Especie.SetItem(1,"espe_codigo", 11)
idwc_especie.SetSort("espe_nombre A")
idwc_especie.Sort()


//Productor
dw_prod.GetChild("prod_codigo", idwc_prod)
idwc_prod.SetTransObject(sqlca)
idwc_prod.Retrieve()
dw_prod.InsertRow(0)
idwc_prod.SetSort("prod_nombre A")
idwc_prod.Sort()

//Embalajes
dw_emba.GetChild("emba_codigo", idwc_emba)
idwc_emba.SetTransObject(sqlca)
idwc_emba.Retrieve(gi_codexport)
dw_emba.InsertRow(0)
idwc_emba.SetSort("ccag_nombre A")
idwc_emba.Sort()

ls_Filtro = "Mid(emba_codigo,1,1) = 'U'" 
idwc_emba.SetFilter(ls_Filtro)
idwc_emba.Filter()

//Variedades
dw_variedad.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(11)
dw_variedad.InsertRow(0)
idwc_variedad.SetSort("vari_nombre A")
idwc_variedad.Sort()
dw_variedad.SetItem(1,"vari_codigo",1)

//Defectos
dw_daños.GetChild("ccde_codigo", idwc_defecto)
idwc_defecto.SetTransObject(sqlca)
idwc_defecto.Retrieve(11)
dw_daños.InsertRow(0)

//Calibres
dw_calibres.InsertRow(0)

em_fechadesde.Text	=	String(RelativeDate(Today(), -1))
em_fechahasta.Text	=	String(Today())
end event

type pb_excel from w_para_informes`pb_excel within w_info_variedades_blancas_objetadas
end type

type st_computador from w_para_informes`st_computador within w_info_variedades_blancas_objetadas
integer x = 1829
end type

type st_usuario from w_para_informes`st_usuario within w_info_variedades_blancas_objetadas
integer x = 1829
end type

type st_temporada from w_para_informes`st_temporada within w_info_variedades_blancas_objetadas
integer x = 1829
end type

type p_logo from w_para_informes`p_logo within w_info_variedades_blancas_objetadas
end type

type st_titulo from w_para_informes`st_titulo within w_info_variedades_blancas_objetadas
integer width = 2057
string text = "Variedades Blancas Objetadas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_variedades_blancas_objetadas
string tag = "Imprimir Reporte"
integer x = 2487
integer y = 856
integer taborder = 270
end type

event pb_acepta::clicked;Integer	li_fila, li_planta, li_Tecnico, li_zona, li_especie, li_variedad, li_Tipo = 0, li_defecto
Long     	ll_prod
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   	ls_filtrolotes, ls_emba, ls_calibr

If cbx_todoszona.Checked Then
  li_zona	= -1
Else
  li_zona	= dw_zona.Object.zona_codigo[1]
  If IsNull(li_zona)Then
	  MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	  Return
  End If
End If

If cbx_todoproduc.Checked Then
	ll_prod	 = -1
Else
	ll_prod	= dw_prod.Object.prod_codigo[1]
	If IsNull(ll_prod) Then
		MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
		Return
	End If
End If
li_especie	= 11

If cbx_todosvari.Checked Then
	li_variedad	 = -1
Else
	li_variedad	= dw_variedad.Object.vari_codigo[1]
	If IsNull(li_variedad) Then
		MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	  	Return
	End If
End If

If cbx_todoemba.Checked Then
	ls_emba	 = '-1'
Else
	ls_emba	= dw_emba.Object.emba_codigo[1]
	If IsNull(ls_emba) Then
		MessageBox("Atención","Debe Seleccionar un Embalaje Previamente",Exclamation!)
	  	Return
	End If
End If	

If cbx_todosagro.Checked Then
	li_Tecnico	 = -1
Else
	li_Tecnico	= dw_tecnico.Object.ccag_codigo[1]
	If IsNull(li_Tecnico)Then
		MessageBox("Atención","Debe Seleccionar un Agronomo Previamente",Exclamation!)
		Return
	End If
End If

If cbx_todoscali.Checked Then
	ls_calibr	 = '-1'
Else
	ls_calibr	= dw_calibres.Object.vaca_calibr[1]
	If IsNull(ls_calibr)Then
		MessageBox("Atención","Debe Seleccionar un Calibre Previamente",Exclamation!)
		Return
	End If
End If

If cbx_todosdefe.Checked Then
	li_defecto	 = -1
Else
	li_defecto	= dw_daños.Object.ccde_codigo[1]
	If IsNull(li_defecto)Then
		MessageBox("Atención","Debe Seleccionar un Defecto Previamente",Exclamation!)
		Return
	End If
End If

If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	RelativeDate(Today(), -1)//Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If
	
istr_info.titulo	= 'VARIEDADES BLANCAS OBJETADAS'
OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_uvasobjetadas_varblancas"
vinf.dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(11)

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(li_zona, ll_prod, li_Tecnico, li_variedad, ls_calibr, &
									  ls_emba, li_defecto, ld_FechaEmbaini, ld_FechaEmbafin)
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

type pb_salir from w_para_informes`pb_salir within w_info_variedades_blancas_objetadas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2491
integer y = 1140
integer taborder = 280
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_variedades_blancas_objetadas
integer x = 411
integer y = 704
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

type st_13 from statictext within w_info_variedades_blancas_objetadas
integer x = 1554
integer y = 1388
integer width = 411
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_variedades_blancas_objetadas
integer x = 1554
integer y = 1448
integer width = 411
integer height = 92
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
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

type st_zona from statictext within w_info_variedades_blancas_objetadas
integer x = 407
integer y = 592
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 588
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
	idwc_planta.Retrieve(1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	dw_planta.InsertRow(0)
	
	RETURN 0
END IF	
end event

event itemerror;Return 1
end event

event clicked;cbx_todoszona.Checked = False
conszona.Checked = False
end event

type cbx_todoplanta from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 704
integer width = 101
integer height = 92
integer taborder = 50
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
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	//dw_planta.Enabled = FALSE	
   dw_planta.SetItem(1, "plde_codigo", li_null)
	consfrigo.Checked = False
ELSE
	//dw_planta.Enabled 		=  TRUE
	dw_planta.Setfocus()
	dw_planta.SetItem(1, "plde_codigo", li_null)
	
END IF
RETURN 0

end event

type dw_planta from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 700
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
consfrigo.Checked = False
end event

type st_33 from statictext within w_info_variedades_blancas_objetadas
integer x = 402
integer y = 816
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
string text = "Técnico"
long bordercolor = 33554431
boolean focusrectangle = false
end type

type dw_tecnico from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 812
integer width = 1024
integer height = 92
integer taborder = 190
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
consagro.Checked = False
end event

type cbx_todoszona from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 588
integer width = 91
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
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
	conszona.Checked = False
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

type cbx_todosagro from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 812
integer width = 91
integer height = 92
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_tecnico.Enabled	=	FALSE	
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
	consagro.Checked = False
ELSE
	//dw_tecnico.Enabled	=	TRUE
	dw_tecnico.Setfocus()
END IF
end event

type cbx_todosfecha from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 1448
integer width = 101
integer height = 92
integer taborder = 220
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
	em_fechadesde.Enabled	=	False
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

type st_12 from statictext within w_info_variedades_blancas_objetadas
integer x = 987
integer y = 1388
integer width = 411
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 983
integer y = 924
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

dw_especie.PostEvent(Clicked!)

IF iuo_especie.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "espe_codigo", Long(ls_nula))
	RETURN 1
ELSE
	dw_variedad.GetChild("vari_codigo", idwc_variedad)
   idwc_variedad.SetTransObject(sqlca)
   idwc_variedad.Retrieve(Integer(Data))
   dw_variedad.InsertRow(0)
	
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

type cbx_espe from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 1847
integer y = 1952
integer width = 279
integer height = 92
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
string text = "Todos"
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	dw_especie.Enabled = FALSE	
   dw_especie.SetItem(1, "espe_codigo", li_null)
ELSE
	dw_especie.Enabled 		=  TRUE
	dw_especie.Setfocus()
	dw_especie.SetItem(1, "espe_codigo", gi_Codespecie)
	
END IF
RETURN 0

end event

type st_3 from statictext within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 407
integer y = 928
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

type st_5 from statictext within w_info_variedades_blancas_objetadas
integer x = 407
integer y = 704
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type cbx_todoproduc from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 700
integer width = 91
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;Long	ll_null

SetNull(ll_Null)

IF This.Checked THEN
	//dw_prod.Enabled = FALSE	
   dw_prod.SetItem(1, "prod_codigo", ll_null)
	consprod.Checked = False
ELSE
	//dw_prod.Enabled 	=  TRUE
	dw_prod.Setfocus()
	dw_prod.SetItem(1, "prod_codigo", ll_null)
	
END IF
RETURN 0

end event

type dw_prod from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 692
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
end event

type st_6 from statictext within w_info_variedades_blancas_objetadas
integer x = 407
integer y = 1264
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
string text = "Defecto"
long bordercolor = 33554431
boolean focusrectangle = false
end type

type cbx_todosdefe from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 1260
integer width = 91
integer height = 92
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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

type ddlb_filtro from dropdownlistbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 773
integer y = 1896
integer width = 1024
integer height = 308
integer taborder = 210
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string item[] = {"Aprobados","Objetados"}
borderstyle borderstyle = stylelowered!
end type

type cbx_todoemba from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 1148
integer width = 91
integer height = 92
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;String	ls_null

SetNull(ls_Null)

IF This.Checked THEN
	//dw_emba.Enabled = FALSE	
   dw_emba.SetItem(1, "emba_codigo", ls_null)
	consemba.Checked = False
ELSE
	//dw_emba.Enabled 	=  TRUE
	dw_emba.Setfocus()
	dw_emba.SetItem(1, "emba_codigo", ls_null)
	
END IF
RETURN 0

end event

type dw_emba from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 1148
integer width = 969
integer height = 92
integer taborder = 160
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
consemba.Checked = False
end event

type st_8 from statictext within w_info_variedades_blancas_objetadas
integer x = 407
integer y = 1152
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_variedades_blancas_objetadas
integer x = 407
integer y = 1452
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_variedades_blancas_objetadas
integer x = 2030
integer y = 528
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
string text = "Todos"
long bordercolor = 33554431
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 777
integer y = 1956
integer width = 402
integer height = 52
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

type st_9 from statictext within w_info_variedades_blancas_objetadas
integer x = 398
integer y = 928
integer width = 306
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
long bordercolor = 33554431
boolean focusrectangle = false
end type

type dw_variedad from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 924
integer width = 891
integer height = 92
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String  ls_Nula
Integer li_especie

dw_variedad.PostEvent(Clicked!)

SetNull(ls_Nula)

li_especie = dw_especie.Object.espe_codigo[1]
IF iuo_variedades.existe(li_especie,Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "vari_codigo", Long(ls_nula))
	RETURN 1
END IF
end event

event itemerror;Return 1
end event

event clicked;cbx_todosvari.Checked 		= 	False
end event

type consfrigo from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 905
integer y = 2000
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
	dw_planta.SetItem(1, "plde_codigo", li_null)
	
END IF
RETURN 0
end event

type consagro from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 2222
integer y = 1952
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
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_tecnico.Enabled	=	FALSE	
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
	cbx_todosagro.Checked = False
ELSE
	//dw_tecnico.Enabled	=	TRUE
	dw_tecnico.Setfocus()
END IF
end event

type cbx_todovariedad from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 1883
integer y = 1944
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
boolean checked = true
end type

event clicked;Long	ll_null

SetNull(ll_Null)

IF This.Checked = True THEN
	consvar.Checked = False
	dw_variedad.SetItem(1, "vari_codigo", ll_null)
ELSE
	dw_variedad.SetFocus()
	dw_variedad.SetItem(1, "vari_codigo", ll_null)
END IF
end event

type rb_consol from radiobutton within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 1225
integer y = 1892
integer width = 773
integer height = 80
integer taborder = 250
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Comparativo Zonas"
end type

event clicked;cbx_todoszona.Checked = False
cbx_todoplanta.Checked = False
cbx_todoproduc.Checked = False
cbx_todovariedad.Checked = False
cbx_todoemba.Checked = False
cbx_todosagro.Checked = False

conszona.Checked = True
consfrigo.Checked = True
consprod.Checked = True
consvar.Checked = True
consemba.Checked = True
consagro.Checked = True

Integer	li_null
Long ll_null
String ls_null
SetNull(li_null)

	dw_zona.SetItem(1, "zona_codigo", li_null)
	dw_planta.SetItem(1, "plde_codigo", li_null)
	dw_prod.SetItem(1, "prod_codigo", li_null)
	dw_variedad.SetItem(1, "vari_codigo", li_null)
	dw_emba.SetItem(1, "emba_codigo", ls_null)
	dw_tecnico.SetItem(1, "ccag_codigo", li_null)
end event

type rb_deta from radiobutton within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 1737
integer y = 1956
integer width = 443
integer height = 80
integer taborder = 260
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Detallado"
boolean checked = true
end type

type em_fechadesde from editmask within w_info_variedades_blancas_objetadas
integer x = 987
integer y = 1448
integer width = 411
integer height = 92
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
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

type st_10 from statictext within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 197
integer y = 1868
integer width = 443
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
string text = "Tipo Informe"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 1134
integer y = 1904
integer width = 2057
integer height = 164
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type conszona from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 905
integer y = 1888
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
	idwc_planta.Retrieve(gi_codexport,1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
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

type consprod from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 905
integer y = 2112
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
long backcolor = 12632256
string text = " "
end type

event clicked;Long	ll_null

SetNull(ll_Null)

IF This.Checked THEN
	//dw_prod.Enabled = FALSE	
   dw_prod.SetItem(1, "prod_codigo", ll_null)
	cbx_todoproduc.Checked = False
ELSE
	//dw_prod.Enabled 	=  TRUE
	dw_prod.Setfocus()
	dw_prod.SetItem(1, "prod_codigo", ll_null)
	
END IF
RETURN 0

end event

type consvar from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 2222
integer y = 1960
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
long backcolor = 12632256
string text = " "
end type

event clicked;Long	ll_null

SetNull(ll_Null)

IF This.Checked = True THEN
	cbx_todovariedad.Checked = False
	dw_variedad.SetItem(1, "vari_codigo", ll_null)
ELSE
	dw_variedad.SetFocus()
	dw_variedad.SetItem(1, "vari_codigo", ll_null)
END IF
end event

type consemba from checkbox within w_info_variedades_blancas_objetadas
boolean visible = false
integer x = 2222
integer y = 1960
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

event clicked;String	ls_null

SetNull(ls_Null)

IF This.Checked THEN
	//dw_emba.Enabled = FALSE	
   dw_emba.SetItem(1, "emba_codigo", ls_null)
	cbx_todoemba.Checked = False
ELSE
	//dw_emba.Enabled 	=  TRUE
	dw_emba.Setfocus()
	dw_emba.SetItem(1, "emba_codigo", ls_null)
	
END IF
RETURN 0
end event

type cbx_todosvari from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 924
integer width = 91
integer height = 92
integer taborder = 180
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

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_tecnico.Enabled	=	FALSE	
	dw_variedad.SetItem(1, "vari_codigo", li_null)
ELSE
	//dw_tecnico.Enabled	=	TRUE
	dw_variedad.Setfocus()
END IF
end event

type st_11 from statictext within w_info_variedades_blancas_objetadas
integer x = 398
integer y = 1048
integer width = 306
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
string text = "Calibre"
long bordercolor = 33554431
boolean focusrectangle = false
end type

type dw_calibres from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 1036
integer width = 919
integer height = 92
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_calibres"
boolean border = false
boolean livescroll = true
end type

event clicked;cbx_todoscali.Checked = False
end event

event itemchanged;String  ls_Nula


end event

event itemerror;Return 1
end event

type dw_daños from datawindow within w_info_variedades_blancas_objetadas
integer x = 983
integer y = 1260
integer width = 969
integer height = 92
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_ctlcaldaños"
boolean border = false
boolean livescroll = true
end type

event clicked;cbx_todosdefe.Checked = False
end event

event itemerror;Return 1
end event

type cbx_todoscali from checkbox within w_info_variedades_blancas_objetadas
integer x = 2094
integer y = 1036
integer width = 91
integer height = 92
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_tecnico.Enabled	=	FALSE	
	dw_variedad.SetItem(1, "vari_codigo", li_null)
ELSE
	//dw_tecnico.Enabled	=	TRUE
	dw_variedad.Setfocus()
END IF
end event

type sle_1 from singlelineedit within w_info_variedades_blancas_objetadas
integer x = 247
integer y = 436
integer width = 2057
integer height = 1252
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

