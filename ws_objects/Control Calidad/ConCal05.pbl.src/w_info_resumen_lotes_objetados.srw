$PBExportHeader$w_info_resumen_lotes_objetados.srw
$PBExportComments$Ventana de Informe de Resumen Lotes Objetados
forward
global type w_info_resumen_lotes_objetados from w_para_informes
end type
type st_1 from statictext within w_info_resumen_lotes_objetados
end type
type em_fechadesde from editmask within w_info_resumen_lotes_objetados
end type
type em_fechahasta from editmask within w_info_resumen_lotes_objetados
end type
type st_zona from statictext within w_info_resumen_lotes_objetados
end type
type dw_zona from datawindow within w_info_resumen_lotes_objetados
end type
type cbx_todoplanta from checkbox within w_info_resumen_lotes_objetados
end type
type dw_planta from datawindow within w_info_resumen_lotes_objetados
end type
type st_33 from statictext within w_info_resumen_lotes_objetados
end type
type dw_productor from datawindow within w_info_resumen_lotes_objetados
end type
type cbx_todoprod from checkbox within w_info_resumen_lotes_objetados
end type
type dw_variedades from datawindow within w_info_resumen_lotes_objetados
end type
type cbx_todosvar from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_todoszona from checkbox within w_info_resumen_lotes_objetados
end type
type st_3 from statictext within w_info_resumen_lotes_objetados
end type
type st_5 from statictext within w_info_resumen_lotes_objetados
end type
type dw_packing from datawindow within w_info_resumen_lotes_objetados
end type
type cbx_todospacking from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_todosfecha from checkbox within w_info_resumen_lotes_objetados
end type
type st_6 from statictext within w_info_resumen_lotes_objetados
end type
type em_embalaje from editmask within w_info_resumen_lotes_objetados
end type
type cb_buscaembalaje from commandbutton within w_info_resumen_lotes_objetados
end type
type em_descripcion from editmask within w_info_resumen_lotes_objetados
end type
type cbx_todosemb from checkbox within w_info_resumen_lotes_objetados
end type
type dw_especie from datawindow within w_info_resumen_lotes_objetados
end type
type st_2 from statictext within w_info_resumen_lotes_objetados
end type
type st_4 from statictext within w_info_resumen_lotes_objetados
end type
type st_7 from statictext within w_info_resumen_lotes_objetados
end type
type cbx_conszonas from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_consplanta from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_conspacking from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_consprod from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_consvariedad from checkbox within w_info_resumen_lotes_objetados
end type
type cbx_consembalaje from checkbox within w_info_resumen_lotes_objetados
end type
type st_8 from statictext within w_info_resumen_lotes_objetados
end type
type st_9 from statictext within w_info_resumen_lotes_objetados
end type
type st_44 from statictext within w_info_resumen_lotes_objetados
end type
type st_10 from statictext within w_info_resumen_lotes_objetados
end type
type st_11 from statictext within w_info_resumen_lotes_objetados
end type
type cbx_consfecha from checkbox within w_info_resumen_lotes_objetados
end type
end forward

global type w_info_resumen_lotes_objetados from w_para_informes
integer x = 14
integer y = 32
integer width = 3067
integer height = 2008
string title = "Resumen Lotes Objetados"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
em_fechadesde em_fechadesde
em_fechahasta em_fechahasta
st_zona st_zona
dw_zona dw_zona
cbx_todoplanta cbx_todoplanta
dw_planta dw_planta
st_33 st_33
dw_productor dw_productor
cbx_todoprod cbx_todoprod
dw_variedades dw_variedades
cbx_todosvar cbx_todosvar
cbx_todoszona cbx_todoszona
st_3 st_3
st_5 st_5
dw_packing dw_packing
cbx_todospacking cbx_todospacking
cbx_todosfecha cbx_todosfecha
st_6 st_6
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
em_descripcion em_descripcion
cbx_todosemb cbx_todosemb
dw_especie dw_especie
st_2 st_2
st_4 st_4
st_7 st_7
cbx_conszonas cbx_conszonas
cbx_consplanta cbx_consplanta
cbx_conspacking cbx_conspacking
cbx_consprod cbx_consprod
cbx_consvariedad cbx_consvariedad
cbx_consembalaje cbx_consembalaje
st_8 st_8
st_9 st_9
st_44 st_44
st_10 st_10
st_11 st_11
cbx_consfecha cbx_consfecha
end type
global w_info_resumen_lotes_objetados w_info_resumen_lotes_objetados

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report

DataWindowChild		idwc_zona, idwc_productores,idwc_planta, idwc_cliente,&
							idwc_variedad,  idwc_inspector, idwc_packing,idwc_especie
uo_zonas					iuo_zonas
uo_ctlcalagronomos	iuo_ctlcalagronomos
uo_especie				iuo_especie


end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexistevariedad (integer variedad)
public subroutine buscaembalaje (integer cliente)
public function boolean noexisteagroproduc (integer ai_agronomo)
public function boolean noexistevariecalibre (integer variedad, string calibre)
public function boolean noexisteproductor (long al_productor)
public function boolean noexisteembalaje (integer cliente, string embalaje)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_cliente,li_zona

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

public function boolean noexistevariedad (integer variedad);Integer li_Contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.variedades
	WHERE vari_codigo = :variedad;

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

public subroutine buscaembalaje (integer cliente);istr_busq.argum[1] = String(gi_CodExport)

OpenWithParm(w_busc_embalajes, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
//	istr_mant.argumento[2] = istr_busq.argum[2]
//	istr_mant.argumento[3] = istr_busq.argum[4]
//	istr_mant.argumento[4] = istr_busq.argum[5]
	em_embalaje.Text	  	  = istr_busq.Argum[2]
	em_descripcion.Text	  = istr_busq.Argum[3]
ELSE
	cb_buscaembalaje.SetFocus()
	
END IF
end subroutine

public function boolean noexisteagroproduc (integer ai_agronomo);Integer li_Contador, li_zona
Long    ll_prod

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

IF cbx_todoprod.Checked = TRUE THEN
	ll_prod = 0
ELSE
	ll_prod	=	dw_productor.object.prod_codigo[1]
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
	messagebox("Atención","Código Productor No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE
END IF

end function

public function boolean noexistevariecalibre (integer variedad, string calibre);String ls_Codigo
Integer li_variedad

Calibre	= Calibre + Fill(" ",3 - Len(Calibre))
ls_codigo	=	''

	SELECT Max(vaca_calibr)
		INTO	:ls_Codigo
		FROM	dbo.variecalibre
		WHERE espe_codigo =  :gi_CodEspecie
		AND   :li_variedad in (0,vari_codigo)
		AND   vaca_calibr	=	:Calibre;
	
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
		RETURN TRUE
	ELSEIF ls_Codigo = '' THEN
		MessageBox("Atencion","Código de Calibre no Existe, Ingrese Otro Código",Exclamation!)
		RETURN TRUE
	ELSE	
		RETURN FALSE	
	END IF

		
end function

public function boolean noexisteproductor (long al_productor);Integer li_Contador, li_zona

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.productores
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

public function boolean noexisteembalaje (integer cliente, string embalaje);String ls_Nombre,ls_Codigo

ls_codigo	=	''

SELECT	emba_codigo,emba_nombre
	INTO	:ls_Codigo,:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE clie_codigo =  :cliente
	AND   emba_codigo = :embalaje;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajes")
	RETURN TRUE
ELSEIF ls_Codigo = '' THEN
	MessageBox("Atencion","Código de Embalaje no Existe, Ingrese Otro Código",Exclamation!)
	RETURN TRUE
ELSE	
	em_descripcion.text	=	ls_Nombre
	RETURN FALSE	
END IF
	


end function

on w_info_resumen_lotes_objetados.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fechadesde=create em_fechadesde
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.dw_zona=create dw_zona
this.cbx_todoplanta=create cbx_todoplanta
this.dw_planta=create dw_planta
this.st_33=create st_33
this.dw_productor=create dw_productor
this.cbx_todoprod=create cbx_todoprod
this.dw_variedades=create dw_variedades
this.cbx_todosvar=create cbx_todosvar
this.cbx_todoszona=create cbx_todoszona
this.st_3=create st_3
this.st_5=create st_5
this.dw_packing=create dw_packing
this.cbx_todospacking=create cbx_todospacking
this.cbx_todosfecha=create cbx_todosfecha
this.st_6=create st_6
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.em_descripcion=create em_descripcion
this.cbx_todosemb=create cbx_todosemb
this.dw_especie=create dw_especie
this.st_2=create st_2
this.st_4=create st_4
this.st_7=create st_7
this.cbx_conszonas=create cbx_conszonas
this.cbx_consplanta=create cbx_consplanta
this.cbx_conspacking=create cbx_conspacking
this.cbx_consprod=create cbx_consprod
this.cbx_consvariedad=create cbx_consvariedad
this.cbx_consembalaje=create cbx_consembalaje
this.st_8=create st_8
this.st_9=create st_9
this.st_44=create st_44
this.st_10=create st_10
this.st_11=create st_11
this.cbx_consfecha=create cbx_consfecha
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fechadesde
this.Control[iCurrent+3]=this.em_fechahasta
this.Control[iCurrent+4]=this.st_zona
this.Control[iCurrent+5]=this.dw_zona
this.Control[iCurrent+6]=this.cbx_todoplanta
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_33
this.Control[iCurrent+9]=this.dw_productor
this.Control[iCurrent+10]=this.cbx_todoprod
this.Control[iCurrent+11]=this.dw_variedades
this.Control[iCurrent+12]=this.cbx_todosvar
this.Control[iCurrent+13]=this.cbx_todoszona
this.Control[iCurrent+14]=this.st_3
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.dw_packing
this.Control[iCurrent+17]=this.cbx_todospacking
this.Control[iCurrent+18]=this.cbx_todosfecha
this.Control[iCurrent+19]=this.st_6
this.Control[iCurrent+20]=this.em_embalaje
this.Control[iCurrent+21]=this.cb_buscaembalaje
this.Control[iCurrent+22]=this.em_descripcion
this.Control[iCurrent+23]=this.cbx_todosemb
this.Control[iCurrent+24]=this.dw_especie
this.Control[iCurrent+25]=this.st_2
this.Control[iCurrent+26]=this.st_4
this.Control[iCurrent+27]=this.st_7
this.Control[iCurrent+28]=this.cbx_conszonas
this.Control[iCurrent+29]=this.cbx_consplanta
this.Control[iCurrent+30]=this.cbx_conspacking
this.Control[iCurrent+31]=this.cbx_consprod
this.Control[iCurrent+32]=this.cbx_consvariedad
this.Control[iCurrent+33]=this.cbx_consembalaje
this.Control[iCurrent+34]=this.st_8
this.Control[iCurrent+35]=this.st_9
this.Control[iCurrent+36]=this.st_44
this.Control[iCurrent+37]=this.st_10
this.Control[iCurrent+38]=this.st_11
this.Control[iCurrent+39]=this.cbx_consfecha
end on

on w_info_resumen_lotes_objetados.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fechadesde)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.dw_zona)
destroy(this.cbx_todoplanta)
destroy(this.dw_planta)
destroy(this.st_33)
destroy(this.dw_productor)
destroy(this.cbx_todoprod)
destroy(this.dw_variedades)
destroy(this.cbx_todosvar)
destroy(this.cbx_todoszona)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.dw_packing)
destroy(this.cbx_todospacking)
destroy(this.cbx_todosfecha)
destroy(this.st_6)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.em_descripcion)
destroy(this.cbx_todosemb)
destroy(this.dw_especie)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.cbx_conszonas)
destroy(this.cbx_consplanta)
destroy(this.cbx_conspacking)
destroy(this.cbx_consprod)
destroy(this.cbx_consvariedad)
destroy(this.cbx_consembalaje)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.st_44)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.cbx_consfecha)
end on

event open;call super::open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

iuo_zonas					=  Create	uo_zonas
iuo_ctlcalagronomos		=  Create	uo_ctlcalagronomos
iuo_especie					=	Create	uo_especie
//zona
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
idwc_zona.SetSort("zona_nombre asc")
idwc_zona.Sort()

//Especie
dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
idwc_planta.SetSort("espe_nombre A")
idwc_planta.Sort()
dw_Especie.InsertRow(0)


//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1,0)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_planta.InsertRow(0)

//Productor
dw_productor.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(0)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_productor.InsertRow(0)

//Variedades
dw_variedades.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(gi_CodEspecie)
dw_variedades.InsertRow(0)
idwc_variedad.SetSort("vari_nombre asc")
idwc_variedad.Sort()

//Packing
dw_packing.GetChild("plde_codigo",idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2,0) 
dw_packing.InsertRow(0)
idwc_packing.SetSort("plde_nombre A")
idwc_packing.Sort()	

em_fechadesde.text	=	'01/11/2003'
em_fechahasta.text	=	String(Today())

dw_Especie.SetItem(1,"espe_codigo",11)

end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_lotes_objetados
integer x = 2711
integer y = 1208
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_lotes_objetados
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_lotes_objetados
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_lotes_objetados
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_lotes_objetados
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_lotes_objetados
integer width = 2391
string text = "Resumen Lotes Objetados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_lotes_objetados
string tag = "Imprimir Reporte"
integer x = 2729
integer y = 512
integer taborder = 240
end type

event pb_acepta::clicked;Integer	li_fila, li_planta, li_Variedad, li_Packing, li_especie, li_zona
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_embalaje,setting
Long     ll_Productor

If cbx_conszonas.Checked  Then
	li_zona	= -9
ElseIf cbx_todoszona.Checked Then
	li_zona	= -1
Else
	li_zona	= dw_zona.Object.zona_codigo[1]
	If IsNull(li_zona)Then
		MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	   Return
	End If
End If

If cbx_consplanta.Checked Then
	li_planta	 = -9
ElseIf cbx_todoplanta.Checked Then 
	li_planta	 = -1
Else	
	li_planta	 = dw_planta.Object.plde_codigo[1]
	If IsNull(li_planta)Then
		MessageBox("Atención","Debe Seleccionar una Planta Previamente",Exclamation!)
	   Return
	End If
End If

If cbx_consprod.Checked Then
	ll_productor	 = -9
ElseIf cbx_todoprod.Checked Then 
	ll_productor	 = -1
Else
	ll_productor	= dw_productor.Object.prod_codigo[1]
	If IsNull(ll_productor)Then
		MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
	   Return
	End If
End If

If IsNull(dw_especie.Object.espe_codigo[1])Then
		MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	   Return
Else
	li_especie = dw_especie.Object.espe_codigo[1]
End If

If cbx_consvariedad.Checked Then
	li_variedad	 = -9
ElseIf cbx_todosvar.Checked Then 
	li_variedad	 = -1
Else	
	If IsNull(dw_variedades.Object.vari_codigo[1])Then
		MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	   Return
	Else
		li_variedad  = dw_variedades.Object.vari_codigo[1]
	End If
End If

If cbx_conspacking.Checked Then
	li_packing	 = -9
ElseIf cbx_todospacking.Checked Then
	li_packing	 = -1
Else
	li_packing	=	dw_packing.Object.plde_codigo[1]
	If IsNull(li_Packing)Then
		MessageBox("Atención","Debe Seleccionar un Packing Previamente",Exclamation!)
		Return
	End If
End If

If cbx_consfecha.Checked Then
	ld_FechaEmbaini 	=	Date(01/01/2000)
	ld_FechaEmbafin 	=	Today()
Else
	ld_FechaEmbaini 	= Date(em_fechadesde.Text)
	ld_FechaEmbafin 	= Date(em_fechahasta.Text)
End If

If cbx_consembalaje.Checked Then
	ls_embalaje	= '**'
ElseIf cbx_todosemb.Checked Then
	ls_embalaje = '*' 
Else
	ls_embalaje	= em_embalaje.Text
	If IsNull(ls_embalaje) Or ls_embalaje = "" Then
		MessageBox("Atención","Debe Digitar un Embalaje Previamente",Exclamation!)
		Return
	End If
End If

istr_info.titulo	= 'RESUMEN DE LOTES OBJETADOS'
OpenWithParm(vinf,istr_info)

If li_especie = 11 Then
	vinf.dw_1.DataObject = "dw_info_resumen_lotes_objetados"
Else
   vinf.dw_1.DataObject = "dw_info_resumenlotesobjetados_esp"
End If

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_codexport, li_zona, li_Planta, li_Packing,ll_productor,&
                             li_especie, li_Variedad,ld_FechaEmbaini, ld_FechaEmbafin, ls_embalaje)

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_fechahasta.text + "'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_lotes_objetados
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2725
integer y = 792
integer taborder = 250
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 900
integer width = 311
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Frigorífico"
boolean focusrectangle = false
end type

type em_fechadesde from editmask within w_info_resumen_lotes_objetados
integer x = 887
integer y = 1504
integer width = 485
integer height = 92
integer taborder = 220
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

type em_fechahasta from editmask within w_info_resumen_lotes_objetados
integer x = 1472
integer y = 1504
integer width = 485
integer height = 92
integer taborder = 230
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

type st_zona from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 548
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Zona Origen"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_resumen_lotes_objetados
integer x = 887
integer y = 544
integer width = 841
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
	dw_Packing.GetChild("plde_codigo", idwc_packing)
	idwc_packing.SetTransObject(sqlca)
	IF idwc_packing.Retrieve(2,Integer(Data)) = 0 THEN
		dw_Packing.SetItem(1, "plde_codigo", Long(ls_Nula))
	END IF	
	
	
	dw_productor.GetChild("prod_codigo", idwc_productores)
	idwc_productores.SetTransObject(sqlca)
	idwc_productores.Retrieve(Integer(Data))
	dw_productor.SetItem(1, "prod_codigo", Long(ls_Nula))	
	
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

event clicked;cbx_todoszona.Checked  = False
cbx_conszonas.Checked  = False
end event

type cbx_todoplanta from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 904
integer width = 78
integer height = 64
integer taborder = 110
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

SetNull(li_Null)

IF This.Checked THEN
	cbx_consplanta.Checked	=	False
	dw_planta.SetItem(1, "plde_codigo", li_null)
ELSE

	dw_planta.Setfocus()
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
END IF
RETURN 0



end event

type dw_planta from datawindow within w_info_resumen_lotes_objetados
integer x = 887
integer y = 892
integer width = 969
integer height = 92
integer taborder = 120
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

event clicked;cbx_consplanta.Checked	=	FALSE
cbx_todoplanta.Checked	=	False	
end event

type st_33 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 668
integer width = 306
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_resumen_lotes_objetados
integer x = 887
integer y = 660
integer width = 955
integer height = 92
integer taborder = 60
boolean bringtotop = true
string dataobject = "dddw_productores_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula

dw_productor.PostEvent(Clicked!)

SetNull(ls_Nula)
 IF NoExisteProductor(Long(data)) THEN
    This.SetItem(1,"prod_codigo", Long(ls_nula))
    RETURN 1
ELSE
	//idwc_tecnico.Retrieve(Long(data),Integer(dw_zona.object.zona_codigo[1]))
END IF	


end event

event itemerror;Return 1
end event

event clicked;cbx_todoprod.Checked = False
cbx_consprod.Checked = False
end event

type cbx_todoprod from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 672
integer width = 78
integer height = 64
integer taborder = 50
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
Setnull(li_null)
IF This.Checked THEN	
	cbx_consprod.Checked	=	False
	dw_productor.SetItem(1, "prod_codigo", Long(li_null))
ELSE
	dw_productor.Setfocus()
END IF


end event

type dw_variedades from datawindow within w_info_resumen_lotes_objetados
integer x = 887
integer y = 1124
integer width = 882
integer height = 92
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_variedades.PostEvent(Clicked!)

IF NoExisteVariedad(Integer(data)) THEN
  THIS.SetItem(1, "vari_codigo", Long(ls_nula))
  RETURN 1
ELSE
 // em_calibre.Text	=	''
  RETURN 0
END IF  
end event

event clicked;cbx_consvariedad.Checked		=	FALSE
cbx_todosvar.Checked	=	False
end event

event itemerror;Return 1
end event

type cbx_todosvar from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 1136
integer width = 78
integer height = 64
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

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	cbx_consvariedad.Checked		=	False
	dw_variedades.SetItem(1, "vari_codigo", li_Null)
ELSE
	cbx_consvariedad.Checked		=	FALSE
	dw_variedades.Setfocus()
END IF
end event

type cbx_todoszona from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 556
integer width = 78
integer height = 64
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
	cbx_conszonas.Checked	=	False
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	/*Packing*/
	idwc_packing.Retrieve(2,0)
	/*Productores*/
	idwc_productores.Retrieve(0)
ELSE

	/*Planta*/
	idwc_planta.Retrieve(1,gi_codzona)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	/*Packing*/
	idwc_packing.Retrieve(2,gi_codzona)
	/*Productores*/
	idwc_productores.Retrieve(gi_codzona)

	dw_zona.Setfocus()
END IF
return 0

end event

type st_3 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 1136
integer width = 343
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 780
integer width = 270
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Packing"
boolean focusrectangle = false
end type

type dw_packing from datawindow within w_info_resumen_lotes_objetados
integer x = 887
integer y = 776
integer width = 978
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantapacking_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_packing.PostEvent(Clicked!)

IF NoExistePlanta(Integer(data),2) THEN
	This.SetItem(1, "plde_codigo", Long(ls_nula))
	RETURN 1
END IF	

end event

event clicked;cbx_todospacking.Checked	=	False
cbx_conspacking.Checked = False
end event

event itemerror;RETURN 1
end event

type cbx_todospacking from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 788
integer width = 73
integer height = 64
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

event clicked;Integer	li_Null

SetNull(li_null)
IF THIS.Checked THEN
	cbx_conspacking.Checked	=	False
	dw_packing.SetItem(1, "plde_codigo",li_Null)
ELSE

	dw_packing.Setfocus()
END IF
end event

type cbx_todosfecha from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 1520
integer width = 78
integer height = 64
integer taborder = 210
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
	em_fechadesde.Enabled		=	False
	em_fechahasta.Enabled	=	False
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	cbx_consfecha.Checked	=	False
ELSE
	//cbx_consfecha.Enabled	=	FALSE
	cbx_consfecha.Checked	=	FALSE
	em_fechadesde.Enabled	   =	TRUE
	em_fechahasta.Enabled	=	TRUE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0
end event

type st_6 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 1340
integer width = 274
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_resumen_lotes_objetados
integer x = 887
integer y = 1336
integer width = 293
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~3"
end type

event modified;IF NoExisteEmbalaje(gi_codexport,This.Text) THEN
	This.Text	=	''
	//em_embalaje.SetFocus()
	RETURN 

END IF
end event

type cb_buscaembalaje from commandbutton within w_info_resumen_lotes_objetados
integer x = 1189
integer y = 1336
integer width = 105
integer height = 88
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;buscaembalaje(gi_CodExport) 
end event

type em_descripcion from editmask within w_info_resumen_lotes_objetados
integer x = 1303
integer y = 1336
integer width = 805
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~3"
end type

type cbx_todosemb from checkbox within w_info_resumen_lotes_objetados
integer x = 2153
integer y = 1348
integer width = 78
integer height = 64
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

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	FALSE	
	cb_buscaembalaje.Enabled	=	FALSE
	cbx_consembalaje.Checked	=	False
	em_descripcion.Text = " "
ELSE
	cbx_consembalaje.Checked	=	FALSE
	em_embalaje.Enabled 			=  TRUE
	em_embalaje.SetFocus()
	cb_buscaembalaje.Enabled	=	TRUE
END IF

end event

type dw_especie from datawindow within w_info_resumen_lotes_objetados
integer x = 887
integer y = 1008
integer width = 891
integer height = 92
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

IF iuo_especie.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "espe_codigo", Long(ls_nula))
	RETURN 1
ELSE
	dw_variedades.GetChild("vari_codigo", idwc_variedad)
   idwc_variedad.SetTransObject(sqlca)
   IF  idwc_variedad.Retrieve(Integer(Data)) = 0 THEN 
	 dw_variedades.SetItem(1, "vari_codigo", Integer(ls_Nula))
   END IF 

END IF


end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 1012
integer width = 251
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_resumen_lotes_objetados
integer x = 887
integer y = 1444
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Desde"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumen_lotes_objetados
integer x = 1477
integer y = 1444
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Hasta"
alignment alignment = center!
boolean focusrectangle = false
end type

type cbx_conszonas from checkbox within w_info_resumen_lotes_objetados
integer x = 2395
integer y = 556
integer width = 78
integer height = 64
integer taborder = 10
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

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todoszona.Checked	=	False	

	dw_zona.SetItem(1, "zona_codigo", Long(ls_nula))
	
ELSE
	dw_zona.SetFocus()
END IF
RETURN 0
end event

type cbx_consplanta from checkbox within w_info_resumen_lotes_objetados
integer x = 2395
integer y = 904
integer width = 78
integer height = 64
integer taborder = 100
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

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todoplanta.Checked	=	False	
	dw_planta.SetItem(1, "plde_codigo", Long(ls_nula))
ELSE
	dw_planta.SetFocus()
END IF
RETURN 0
end event

type cbx_conspacking from checkbox within w_info_resumen_lotes_objetados
integer x = 2395
integer y = 788
integer width = 78
integer height = 64
integer taborder = 70
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

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todospacking.Checked	=	False	
	dw_packing.SetItem(1, "plde_codigo", Long(ls_nula))
ELSE
	dw_packing.SetFocus()
END IF
RETURN 0
end event

type cbx_consprod from checkbox within w_info_resumen_lotes_objetados
integer x = 2395
integer y = 672
integer width = 78
integer height = 64
integer taborder = 40
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

event clicked;String ls_Nula
SetNull(ls_Nula)
IF THIS.CheCked THEN
	cbx_todoprod.Checked	=	False	
	dw_productor.SetItem(1, "prod_codigo", Long(ls_nula))
ELSE
	dw_productor.SetFocus()
END IF

end event

type cbx_consvariedad from checkbox within w_info_resumen_lotes_objetados
integer x = 2395
integer y = 1136
integer width = 78
integer height = 64
integer taborder = 140
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

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todosvar.Checked	=	False	
	dw_variedades.SetItem(1, "vari_codigo", Long(ls_nula))
ELSE
	dw_variedades.SetFocus()
END IF
RETURN 0
end event

type cbx_consembalaje from checkbox within w_info_resumen_lotes_objetados
integer x = 2395
integer y = 1348
integer width = 78
integer height = 64
integer taborder = 170
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

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	FALSE	
	cb_buscaembalaje.Enabled	=	FALSE
	cbx_todosemb.Checked	=	FALSE
	em_descripcion.Text = " "
ELSE
	em_embalaje.Enabled 			=  TRUE
	em_embalaje.SetFocus()
	cb_buscaembalaje.Enabled	=	TRUE
END IF
end event

type st_8 from statictext within w_info_resumen_lotes_objetados
integer x = 256
integer y = 1280
integer width = 2391
integer height = 420
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

type st_9 from statictext within w_info_resumen_lotes_objetados
integer x = 352
integer y = 1508
integer width = 485
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_resumen_lotes_objetados
integer x = 251
integer y = 440
integer width = 2391
integer height = 840
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_resumen_lotes_objetados
integer x = 2094
integer y = 480
integer width = 197
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_resumen_lotes_objetados
integer x = 2331
integer y = 484
integer width = 201
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

type cbx_consfecha from checkbox within w_info_resumen_lotes_objetados
boolean visible = false
integer x = 2048
integer y = 1752
integer width = 279
integer height = 64
integer taborder = 270
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
string text = " "
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled		=	False
	em_fechahasta.Enabled	=	False
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	cbx_todosfecha.Checked	=	FALSE
ELSE
	em_fechadesde.Enabled	   =	TRUE
	em_fechahasta.Enabled	=	TRUE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0
end event

