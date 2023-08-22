$PBExportHeader$w_mant_mantienepaltrans.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_mantienepaltrans from w_mant_directo
end type
type st_2 from statictext within w_mant_mantienepaltrans
end type
type st_1 from statictext within w_mant_mantienepaltrans
end type
type em_numero from editmask within w_mant_mantienepaltrans
end type
type st_3 from statictext within w_mant_mantienepaltrans
end type
type dw_cliente from datawindow within w_mant_mantienepaltrans
end type
type dw_4 from datawindow within w_mant_mantienepaltrans
end type
type pb_recupera from picturebutton within w_mant_mantienepaltrans
end type
type dw_3 from datawindow within w_mant_mantienepaltrans
end type
type st_5 from statictext within w_mant_mantienepaltrans
end type
type st_6 from statictext within w_mant_mantienepaltrans
end type
type st_7 from statictext within w_mant_mantienepaltrans
end type
type dw_etiqueta from datawindow within w_mant_mantienepaltrans
end type
type dw_status from datawindow within w_mant_mantienepaltrans
end type
type dw_tipopallemba from datawindow within w_mant_mantienepaltrans
end type
type st_8 from statictext within w_mant_mantienepaltrans
end type
type dw_codigopallet from datawindow within w_mant_mantienepaltrans
end type
end forward

global type w_mant_mantienepaltrans from w_mant_directo
integer x = 155
integer y = 156
integer width = 4709
integer height = 2148
string title = "MANTENCION DETALLE PALLET TRANSITORIOS"
boolean minbox = false
boolean maxbox = false
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
em_numero em_numero
st_3 st_3
dw_cliente dw_cliente
dw_4 dw_4
pb_recupera pb_recupera
dw_3 dw_3
st_5 st_5
st_6 st_6
st_7 st_7
dw_etiqueta dw_etiqueta
dw_status dw_status
dw_tipopallemba dw_tipopallemba
st_8 st_8
dw_codigopallet dw_codigopallet
end type
global w_mant_mantienepaltrans w_mant_mantienepaltrans

type variables
DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_etiqueta, idwc_status, idwc_tipopallemba, idwc_codigopallet
Integer ii_pallet, ii_nuevo

//str_busqueda istr_busq
//str_mant istr_mant
Integer	ii_tipo, ii_contador
String	is_report

uo_plantadesp			iuo_Planta
uo_clientesprod			iuo_ClienteProd



end variables

forward prototypes
public function boolean noexistevariedad (string as_columna, string as_valor)
public function string f_variedadnom (integer codigo)
public function string f_prodnombre (integer li_codigo)
public subroutine buscavariedad ()
public function boolean existepallet (long al_numero)
public function boolean existeembalaje (string as_embalaje, integer al_cliente)
protected function boolean wf_actualiza_db ()
public function boolean existeproductor (long al_productor)
public function boolean existepredio (long al_productor, integer ai_predio)
public function boolean existecuartel (long al_productor, integer ai_predio, integer ai_cuartel)
public function boolean existecalibre (integer ai_variedad, integer ai_especie, string as_calibre)
public function boolean exietepacking (integer ai_planta)
public function boolean existecategoria (integer categoria)
end prototypes

public function boolean noexistevariedad (string as_columna, string as_valor);Integer	li_Especie, li_Variedad, li_Existes
Boolean	lb_Retorno

li_Especie	=	dw_1.Object.espe_codigo[il_Fila]

CHOOSE CASE as_Columna
	CASE "pafr_varrot"
		li_Variedad	=	Integer(as_Valor)
		
	CASE "vari_codigo"
		li_Variedad	=	Integer(as_Valor)	
	
END CHOOSE

SELECT Count(*)
	INTO	 :li_Existes
	FROM	 dbo.variedades
	WHERE  vari_codigo	=	:li_Variedad
	AND    espe_codigo	=	:li_Especie;
	
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Variedades")
	ELSEIF 	li_Existes	=	0	THEN
		MessageBox("Atención","Código de Variedad no Existe.~r " +&
		"Ingrese Otro.")			
		lb_Retorno	=	TRUE
	END IF
	

RETURN lb_Retorno
end function

public function string f_variedadnom (integer codigo);string ls_nombre
integer li_cliente, li_especie

li_cliente = dw_1.Object.clie_codigo[il_fila]
li_especie = dw_1.Object.espe_codigo[il_fila]

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dbo.variedades
	WHERE	vari_codigo	=	:codigo and
		   espe_codigo =	:li_especie;
		

	RETURN ls_nombre

end function

public function string f_prodnombre (integer li_codigo);string ls_nombre
integer li_cliente, li_especie

li_cliente = dw_1.Object.clie_codigo[il_fila]


SELECT	prod_nombre
	INTO	:ls_nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:li_codigo;
		
RETURN ls_nombre



end function

public subroutine buscavariedad ();string 	ls_null

SetNull(ls_null)

Str_busqueda	lstr_busq

dw_1.Modify("buscavariedad.border = 0")
dw_1.Modify("buscavariedad.border = 5")

lstr_busq.argum[1]	=	istr_mant.Argumento[1]
lstr_busq.argum[2]	=	String(dw_1.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[5]
	
	dw_1.setItem(il_fila, "pafr_varrot", Integer(lstr_busq.argum[4]))
	dw_1.setItem(il_fila, "vari_nombre_rotula", lstr_busq.argum[5])
	dw_1.SetColumn("pafr_varrot")
	dw_1.SetFocus()
END IF

end subroutine

public function boolean existepallet (long al_numero);Integer	li_codexp, li_planta, li_Tipova, li_etiqueta, li_status, li_tipopa, li_copacodigo
Date		ld_fecha
String	ls_Embarque, ls_embalaje, ls_tipopallemba
Long		ll_pallet

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_numero <> 0 OR li_planta = 0 THEN

		SELECT paen_numero,tpem_codigo,etiq_codigo,stat_codigo,paen_tipopa, emba_codigo, copa_codigo
		INTO	:ll_pallet,:ls_tipopallemba, :li_etiqueta, :li_status, :li_tipopa, :ls_embalaje, :li_copacodigo
		FROM	dbo.palletencab_trans
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	paen_numero	=	:al_numero
		AND	paen_Estado =	1;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla palletencab_trans")
		em_numero.Text = ''
		em_numero.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Número de Pallet No Está en Existencia.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.Text = ''
		em_numero.SetFocus()
		RETURN False
	END IF

	idwc_tipopallemba.Retrieve(li_codexp,ls_embalaje)
	
	dw_etiqueta.Object.etiq_codigo[1]		=	li_etiqueta
	dw_status.Object.stat_codigo[1]			=	li_status
	dw_codigopallet.Object.copa_codigo[1]	=	li_copacodigo
	
	IF li_tipopa = 1 THEN
		dw_tipopallemba.Object.tpem_codigo[1]	=	ls_tipopallemba	
		dw_tipopallemba.Enabled	=	True
	ELSE
		dw_tipopallemba.Enabled	=	False
	END IF
	dw_etiqueta.Enabled	=	True
	dw_status.Enabled	=	True
	dw_codigopallet.Enabled	=	True

	RETURN TRUE
END IF	
	
end function

public function boolean existeembalaje (string as_embalaje, integer al_cliente);Integer	li_Existes
Boolean	lb_Retorno=FALSE


SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.embalajesprod
WHERE  emba_codigo	=	:as_embalaje
AND	 clie_codigo 	= 	:al_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de embalajesprod")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Embalaje No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora
Long		ll_pallet
Integer	li_cliente, li_planta, li_etiqueta, li_status, li_copacodigo
String	ls_tipopallemba


li_cliente	=	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])
ll_pallet	=	Long(istr_mant.argumento[6])

li_etiqueta			=	dw_etiqueta.Object.etiq_codigo[1]
li_status			=	dw_status.Object.stat_codigo[1]
ls_tipopallemba	=	dw_tipopallemba.Object.tpem_codigo[1]
li_copacodigo		=	dw_codigopallet.Object.copa_codigo[1]

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		
		update dbo.palletencab_trans set
		etiq_codigo = :li_etiqueta,
		stat_codigo = :li_status,
		tpem_codigo = :ls_tipopallemba,
		copa_codigo = :li_copacodigo
		where clie_codigo=:li_cliente
		and   plde_codigo=:li_planta
		and   paen_numero=:ll_pallet;
		Commit;
		update dbo.palletfruta_trans set
		etiq_codigo = :li_etiqueta
		where clie_codigo=:li_cliente
		and   plde_codigo=:li_planta
		and   paen_numero=:ll_pallet;
		Commit;
		update dbo.palletencab set
		etiq_codigo = :li_etiqueta,
		stat_codigo = :li_status,
		tpem_codigo = :ls_tipopallemba,
		copa_codigo = :li_copacodigo
		where clie_codigo=:li_cliente
		and   plde_codigo=:li_planta
		and   paen_numero=:ll_pallet;
		Commit;
	
				
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existeproductor (long al_productor);Integer	li_Existes
Boolean	lb_Retorno=FALSE

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.productores
WHERE  prod_codigo	=	:al_productor;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de productores")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Productor No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public function boolean existepredio (long al_productor, integer ai_predio);Integer	li_Existes
Boolean	lb_Retorno=FALSE

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.spro_prodpredio
WHERE  prod_codigo =	:al_productor
AND prpr_codigo = :ai_predio;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de spro_prodpredio")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Predio No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public function boolean existecuartel (long al_productor, integer ai_predio, integer ai_cuartel);Integer	li_Existes
Boolean	lb_Retorno=FALSE

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.spro_prodcuarteles
WHERE  prod_codigo =	:al_productor
AND prpr_codigo = :ai_predio
AND prcc_codigo = :ai_cuartel;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de spro_prodcuarteles")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Cuartel No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public function boolean existecalibre (integer ai_variedad, integer ai_especie, string as_calibre);Integer	li_Especie, li_Variedad, li_Existes
Boolean	lb_Retorno

SELECT Count(*)
	INTO	 :li_Existes
	FROM	 dbo.variecalibre
	WHERE vari_codigo	= :ai_Variedad
	AND espe_codigo = :ai_Especie
	AND vaca_calibr = :as_calibre;
	
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de variecalibre")
	ELSEIF 	li_Existes	=	0	THEN
		MessageBox("Atención","Código de Calibre no Existe.~r " +&
		"Ingrese Otro.")			
		lb_Retorno	=	TRUE
	END IF
	

RETURN lb_Retorno
end function

public function boolean exietepacking (integer ai_planta);Integer	li_Existes
Boolean	lb_Retorno=FALSE

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.plantadesp
WHERE  plde_codigo	=	:ai_planta;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de plantadesp")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Packing No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public function boolean existecategoria (integer categoria);Integer	li_Existes
Boolean	lb_Retorno=FALSE


SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.categorias
WHERE  cate_codigo = :categoria;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de categorias")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Categoria No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
buscar			= "Código:Ncodigo,Descripción:Sconcepto"
ordenar			= "Código:codigo,Descripción:concepto"
is_ultimacol	= "columna"

ii_tipo	=	Integer(Message.StringParm)

dw_cliente.SetTransObject(sqlca)
dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo", gi_codexport)

dw_4.SetTransObject(sqlca)
dw_4.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(-1)
dw_4.InsertRow(0)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

istr_mant.dw				=	dw_1
istr_mant.dw2				=	dw_3

iuo_Planta				=	Create uo_plantadesp
iuo_ClienteProd		=	Create uo_clientesprod

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve(-1)
dw_etiqueta.InsertRow(0)

dw_status.GetChild("stat_codigo", idwc_status)
idwc_status.SetTransObject(sqlca)
idwc_status.Retrieve(-1)
dw_status.InsertRow(0)

dw_tipopallemba.GetChild("tpem_codigo", idwc_tipopallemba)
idwc_tipopallemba.SetTransObject(sqlca)
idwc_tipopallemba.Retrieve(gi_codexport,'Z')
dw_tipopallemba.InsertRow(0)

dw_codigopallet.GetChild("copa_codigo", idwc_codigopallet)
idwc_codigopallet.SetTransObject(sqlca)
idwc_codigopallet.Retrieve(-1)
dw_codigopallet.InsertRow(0)

dw_etiqueta.Enabled	=	False
dw_status.Enabled	=	False
dw_tipopallemba.Enabled	=	False
dw_codigopallet.Enabled	=	False



end event

on w_mant_mantienepaltrans.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_numero=create em_numero
this.st_3=create st_3
this.dw_cliente=create dw_cliente
this.dw_4=create dw_4
this.pb_recupera=create pb_recupera
this.dw_3=create dw_3
this.st_5=create st_5
this.st_6=create st_6
this.st_7=create st_7
this.dw_etiqueta=create dw_etiqueta
this.dw_status=create dw_status
this.dw_tipopallemba=create dw_tipopallemba
this.st_8=create st_8
this.dw_codigopallet=create dw_codigopallet
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_numero
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.dw_4
this.Control[iCurrent+7]=this.pb_recupera
this.Control[iCurrent+8]=this.dw_3
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.st_7
this.Control[iCurrent+12]=this.dw_etiqueta
this.Control[iCurrent+13]=this.dw_status
this.Control[iCurrent+14]=this.dw_tipopallemba
this.Control[iCurrent+15]=this.st_8
this.Control[iCurrent+16]=this.dw_codigopallet
end on

on w_mant_mantienepaltrans.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.dw_cliente)
destroy(this.dw_4)
destroy(this.pb_recupera)
destroy(this.dw_3)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.dw_etiqueta)
destroy(this.dw_status)
destroy(this.dw_tipopallemba)
destroy(this.st_8)
destroy(this.dw_codigopallet)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta, ll_guiadespacho

DO
	ll_fila	= dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

lstr_info.titulo	= "DETALLE DE PALLET TRANSITORIOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_manttrans_pallet"

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF

	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
		ls_colu[li_cont]	= "pafr_prdrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
		ls_colu[li_cont]	= "pafr_calrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
		ls_colu[li_cont]	= "pafr_huert4"
	END IF	

	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
		ls_colu[li_cont]	= "pafr_cuart4"
	END IF	

	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
		ls_colu[li_cont]	= "pafr_rotpak"
	END IF	
	
	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de fecha embalaje Rotulada"
		ls_colu[li_cont]	= "pafr_fecrot"
	END IF		
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
//		ii_contador = 1
//		
//		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
//			ii_contador++
//		loop 
//		ii_contador --
//		dw_1.SetRow(ii_contador)
//		Message.DoubleParm = -1
	END IF
END IF





end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1
		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Real"
		ls_colu[li_cont]	= "vari_codigo"
	END IF
	
	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Real"
		ls_colu[li_cont]	= "prod_codigo"
	END IF
	
	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
		ls_colu[li_cont]	= "pafr_prdrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calibr[il_fila]) OR dw_1.Object.pafr_calibr[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Real"
		ls_colu[li_cont]	= "pafr_calibr"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
		ls_colu[li_cont]	= "pafr_calrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_huert1[il_fila]) OR dw_1.Object.pafr_huert1[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Real"
		ls_colu[li_cont]	= "pafr_huert1"
	END IF	
	
	IF IsNull(dw_1.Object.pafr_huert1[il_fila]) OR dw_1.Object.pafr_huert1[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Real"
		ls_colu[li_cont]	= "pafr_huert1"
	END IF	
	
	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
		ls_colu[li_cont]	= "pafr_huert4"
	END IF
	
	IF IsNull(dw_1.Object.pafr_cuart1[il_fila]) OR dw_1.Object.pafr_cuart1[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Real"
		ls_colu[li_cont]	= "pafr_cuart1"
	END IF	

	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
		ls_colu[li_cont]	= "pafr_cuart4"
	END IF
	
	IF IsNull(dw_1.Object.pafr_copack[il_fila]) OR dw_1.Object.pafr_copack[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Real"
		ls_colu[li_cont]	= "pafr_copack"
	END IF	

	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
		ls_colu[li_cont]	= "pafr_rotpak"
	END IF	
	
	IF IsNull(dw_1.Object.pafr_embrea[il_fila]) OR dw_1.Object.pafr_embrea[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Embalaje Real"
		ls_colu[li_cont]	= "pafr_embrea"
	END IF
	
	IF IsNull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Embalaje Rotulado"
		ls_colu[li_cont]	= "emba_codigo"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF
END IF





end event

event resize;call super::resize;pb_recupera.x	= pb_Imprimir.x
pb_recupera.y	= pb_Imprimir.y + pb_Imprimir.Height + 10
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mantienepaltrans
integer x = 64
integer y = 28
integer width = 4133
integer height = 356
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mantienepaltrans
integer x = 4334
integer y = 436
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;String	ls_nulo
Integer	li_nulo

SetNull(ls_nulo)
SetNull(li_nulo)

dw_cliente.Enabled			=	TRUE
dw_4.Enabled					=	TRUE
em_numero.Enabled				=	TRUE

dw_cliente.Reset()
dw_cliente.InsertRow(0)
dw_cliente.SetFocus()
dw_cliente.Object.clie_codigo.Background.Color=	RGB(255, 255, 255)
dw_cliente.SetItem(1,"clie_codigo", gi_codexport)

dw_4.Reset()
dw_4.InsertRow(0)
dw_4.Object.plde_codigo.Background.Color=	RGB(255, 255, 255)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

em_numero.BackColor=	RGB(255, 255, 255)
em_numero.text 	=	''

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

dw_status.GetChild("stat_codigo", idwc_status)
idwc_status.SetTransObject(sqlca)
idwc_status.Retrieve()
dw_status.InsertRow(0)

dw_tipopallemba.GetChild("tpem_codigo", idwc_tipopallemba)
idwc_tipopallemba.SetTransObject(sqlca)
idwc_tipopallemba.Retrieve(gi_codexport,'Z')
dw_tipopallemba.InsertRow(0)

dw_codigopallet.GetChild("copa_codigo", idwc_codigopallet)
idwc_codigopallet.SetTransObject(sqlca)
idwc_codigopallet.Retrieve()
dw_codigopallet.InsertRow(0)

dw_etiqueta.Enabled	=	False
dw_status.Enabled	=	False
dw_tipopallemba.Enabled	=	False
dw_codigopallet.Enabled	=	False

dw_etiqueta.Object.etiq_codigo[1]		=	li_nulo
dw_status.Object.stat_codigo[1]			=	li_nulo
dw_tipopallemba.Object.tpem_codigo[1]	=	ls_nulo
dw_codigopallet.Object.copa_codigo[1]			=	li_nulo

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mantienepaltrans
integer x = 4329
integer y = 176
integer taborder = 50
end type

event pb_lectura::clicked;Integer 	li_nombrevari
Long		ll_guiadespacho


IF IsNull(dw_cliente.Object.clie_codigo[1]) OR dw_cliente.Object.clie_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Cliente Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_4.Object.plde_codigo[1]) OR  dw_4.Object.plde_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Planta Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(integer(em_numero.text)) THEN
	MessageBox("Atención","Debe Seleccionar Nº de Pallet Previamente",Exclamation!)
	RETURN
	em_numero.SetFocus()
ELSE
	
	dw_1.DataObject = "dw_mues_mantpaltrans"
	dw_3.DataObject = "dw_mues_mantpaltrans"		
		
	dw_1.SettransObject(sqlca)
	dw_3.SettransObject(sqlca)
	
	dw_cliente.Enabled		=	FALSE
	dw_4.Enabled				=	FALSE
	em_numero.Enabled			=	FALSE
	dw_cliente.Object.clie_codigo.Background.Color=	RGB(166,180,210)
	dw_4.Object.plde_codigo.Background.Color=	RGB(166,180,210)
	em_numero.BackColor=	RGB(166,180,210)
	Parent.PostEvent("ue_recuperadatos")
	
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mantienepaltrans
boolean visible = false
integer x = 4329
integer y = 796
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mantienepaltrans
boolean visible = false
integer x = 4329
integer y = 616
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_mantienepaltrans
integer x = 4329
integer y = 1776
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mantienepaltrans
integer x = 4329
integer y = 1156
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mantienepaltrans
integer x = 4329
integer y = 976
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mantienepaltrans
integer x = 64
integer y = 388
integer width = 4142
integer height = 1620
integer taborder = 60
string dataobject = "dw_mues_mantpaltrans"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

	
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF

IF dw_1.rowcount() > 0 THEN
	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
		dw_1.SetRow(ii_contador)
	END IF
END IF



//CHOOSE CASE ls_Columna	
//		
//	CASE "pafr_varrot"
//		istr_mant.Argumento[2]	=	Data	
//			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
//			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
//			IF NoExisteVariedad(ls_Columna,Data)	THEN
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//	
//END CHOOSE
//
//
//
//
//
end event

event dw_1::itemchanged;call super::itemchanged;String ls_Columna, ls_null, ls_varnom
Integer	li_Nula 


SetNull(li_Nula)
SetNull(ls_null)

dw_1.AcceptText()

ls_Columna	=	Dwo.Name

CHOOSE CASE ls_Columna	
		
	CASE "pafr_varrot"
		IF NoExisteVariedad(ls_Columna,Data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF
			
	CASE "vari_codigo"
		IF NoExisteVariedad(ls_Columna,Data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		
		END IF		
	
	CASE 'emba_codigo'
		IF existeembalaje(Data,dw_cliente.Object.clie_codigo[1])	THEN
			dw_1.SetItem(il_Fila,ls_Columna,String(li_Nula))
			RETURN 1
		END IF
		
	CASE 'pafr_embrea'	
		IF existeembalaje(Data,dw_cliente.Object.clie_codigo[1])	THEN
			dw_1.SetItem(il_Fila,ls_Columna,String(li_Nula))
			RETURN 1
		END IF
		
	CASE 'prod_codigo'	
		IF existeproductor(Long(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Long(li_Nula))
			RETURN 1
		END IF	
		
	CASE 'pafr_prdrot'	
		IF existeproductor(Long(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Long(li_Nula))
			RETURN 1
		END IF	
		
	CASE 'pafr_huert1'	
		IF existepredio(dw_1.Object.prod_codigo[row],integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(li_Nula))
			RETURN 1
		END IF		
		
	CASE 'pafr_huert4'	
		IF existepredio(dw_1.Object.pafr_prdrot[row],integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(li_Nula))
			RETURN 1
		END IF	

	CASE 'pafr_cuart1'	
		IF existecuartel(dw_1.Object.prod_codigo[row],dw_1.Object.pafr_huert1[row],integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(li_Nula))
			RETURN 1
		END IF
		
	CASE 'pafr_cuart4'	
		IF existecuartel(dw_1.Object.pafr_prdrot[row],dw_1.Object.pafr_huert4[row],integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(li_Nula))
			RETURN 1
		END IF	
		
	CASE 'pafr_calibr'	
		IF existecalibre(dw_1.Object.espe_codigo[row],dw_1.Object.vari_codigo[row],Data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF	
		
	CASE 'pafr_calrot'	
		IF existecalibre(dw_1.Object.pafr_calrot[row],dw_1.Object.pafr_calrot[row],Data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF	
		
	CASE 'pafr_copack'	
		IF exietepacking(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF	
		
	CASE 'pafr_rotpak'	
		IF exietepacking(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF	
		
	CASE 'pafr_rotpak'	
		IF existecategoria(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF	
		
	CASE 'pafr_rotpak'	
		IF existecategoria(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF			
	
	END CHOOSE	
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name		
	CASE "buscavariedad"
			buscavariedad()
			
	
END CHOOSE


end event

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type st_2 from statictext within w_mant_mantienepaltrans
integer x = 174
integer y = 156
integer width = 270
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mantienepaltrans
integer x = 174
integer y = 60
integer width = 270
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
string text = "Cliente"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_mantienepaltrans
integer x = 521
integer y = 256
integer width = 402
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[6]=em_numero.text

IF existepallet(Long(em_numero.text)) = False THEN
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_mant_mantienepaltrans
integer x = 174
integer y = 256
integer width = 247
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
string text = "Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_mant_mantienepaltrans
integer x = 517
integer y = 60
integer width = 1161
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF iuo_ClienteProd.existe(Integer(Data), True, sqlca) THEN
	istr_mant.argumento[1]	=	String(data)
	
ELSE
	This.SetItem(1, "clie_codigo", Integer(ll_null))

	RETURN 1
END IF
	

end event

event itemerror;Return 1
end event

type dw_4 from datawindow within w_mant_mantienepaltrans
integer x = 517
integer y = 156
integer width = 965
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null


SetNull(ll_null)

IF Not iuo_Planta.Existe(Integer(Data), True, sqlca) THEN
	This.SetItem(1, "plde_codigo", Integer(ll_null))

	RETURN 1
ELSE			
	istr_mant.argumento[2]=String(data)
END IF		



end event

event itemerror;Return 1
end event

type pb_recupera from picturebutton within w_mant_mantienepaltrans
string tag = "Caja a Caja"
integer x = 4329
integer y = 1428
integer width = 302
integer height = 244
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\apuntes.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\apuntes-bn.png"
alignment htextalign = left!
end type

event clicked;Long	ll_fila_d, ll_guiadespacho

ll_fila_d	= dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
istr_mant.argumento[11]	=	'P'  	//Pallet

IF ll_fila_d > 0 THEN
	MessageBox("Atención", "Los Cambios Surtirán Efecto Solo Cuando Haya Grabado.", &
					Exclamation!, OK!)
	
	OpenWithParm(w_mant_deta_palletmantagrupado, istr_mant)
END IF
end event

type dw_3 from datawindow within w_mant_mantienepaltrans
boolean visible = false
integer x = 2606
integer y = 2124
integer width = 686
integer height = 400
integer taborder = 130
boolean bringtotop = true
string title = "none"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_mant_mantienepaltrans
integer x = 1998
integer y = 60
integer width = 398
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
string text = "Etiqueta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mantienepaltrans
integer x = 1998
integer y = 156
integer width = 398
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
string text = "Status"
boolean focusrectangle = false
end type

type st_7 from statictext within w_mant_mantienepaltrans
integer x = 1998
integer y = 256
integer width = 398
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
string text = "Cód.Altura  "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_etiqueta from datawindow within w_mant_mantienepaltrans
integer x = 2409
integer y = 60
integer width = 942
integer height = 96
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
//IF NoExisteEtiqueta(Integer(data)) THEN
//	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
//	dw_etiqueta.SetFocus()
//	RETURN 1
//ELSE
//	istr_mant.argumento[7]	=	data
//	
//END IF
//

end event

event itemerror;RETURN 1
end event

type dw_status from datawindow within w_mant_mantienepaltrans
integer x = 2409
integer y = 156
integer width = 1029
integer height = 96
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
//IF NoExisteEtiqueta(Integer(data)) THEN
//	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
//	dw_etiqueta.SetFocus()
//	RETURN 1
//ELSE
//	istr_mant.argumento[7]	=	data
//	
//END IF
//
//
end event

event itemerror;RETURN 1
end event

type dw_tipopallemba from datawindow within w_mant_mantienepaltrans
integer x = 2400
integer y = 256
integer width = 352
integer height = 96
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_tipopallemba"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
//IF NoExisteEtiqueta(Integer(data)) THEN
//	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
//	dw_etiqueta.SetFocus()
//	RETURN 1
//ELSE
//	istr_mant.argumento[7]	=	data
//	
//END IF
//
//
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_mant_mantienepaltrans
integer x = 2894
integer y = 256
integer width = 366
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
string text = "Cód.Pallet  "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_codigopallet from datawindow within w_mant_mantienepaltrans
integer x = 3237
integer y = 256
integer width = 754
integer height = 96
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_codigopallet"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
//IF NoExisteEtiqueta(Integer(data)) THEN
//	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
//	dw_etiqueta.SetFocus()
//	RETURN 1
//ELSE
//	istr_mant.argumento[7]	=	data
//	
//END IF
//
//
end event

event itemerror;RETURN 1
end event

