$PBExportHeader$w_gene_despacho_zeal.srw
forward
global type w_gene_despacho_zeal from window
end type
type ddlb_2 from dropdownlistbox within w_gene_despacho_zeal
end type
type dw_5 from datawindow within w_gene_despacho_zeal
end type
type dw_4 from datawindow within w_gene_despacho_zeal
end type
type cbx_2 from checkbox within w_gene_despacho_zeal
end type
type dw_3 from datawindow within w_gene_despacho_zeal
end type
type st_11 from statictext within w_gene_despacho_zeal
end type
type st_10 from statictext within w_gene_despacho_zeal
end type
type cbx_validadestino from checkbox within w_gene_despacho_zeal
end type
type cbx_1 from checkbox within w_gene_despacho_zeal
end type
type ddlb_1 from dropdownlistbox within w_gene_despacho_zeal
end type
type st_7 from statictext within w_gene_despacho_zeal
end type
type cbx_var from checkbox within w_gene_despacho_zeal
end type
type em_fecha from editmask within w_gene_despacho_zeal
end type
type dw_11 from datawindow within w_gene_despacho_zeal
end type
type dw_10 from datawindow within w_gene_despacho_zeal
end type
type st_4 from statictext within w_gene_despacho_zeal
end type
type st_3 from statictext within w_gene_despacho_zeal
end type
type sle_mensa from singlelineedit within w_gene_despacho_zeal
end type
type em_planilla from editmask within w_gene_despacho_zeal
end type
type st_5 from statictext within w_gene_despacho_zeal
end type
type st_2 from statictext within w_gene_despacho_zeal
end type
type st_1 from statictext within w_gene_despacho_zeal
end type
type pb_salir from picturebutton within w_gene_despacho_zeal
end type
type pb_grabar from picturebutton within w_gene_despacho_zeal
end type
type gb_2 from groupbox within w_gene_despacho_zeal
end type
type gb_1 from groupbox within w_gene_despacho_zeal
end type
type st_6 from statictext within w_gene_despacho_zeal
end type
type dw_1 from datawindow within w_gene_despacho_zeal
end type
type dw_2 from datawindow within w_gene_despacho_zeal
end type
type st_8 from statictext within w_gene_despacho_zeal
end type
type st_9 from statictext within w_gene_despacho_zeal
end type
end forward

global type w_gene_despacho_zeal from window
integer width = 4434
integer height = 1936
boolean titlebar = true
string title = "GENERA DESPACHO ZEAL"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 30586022
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
ddlb_2 ddlb_2
dw_5 dw_5
dw_4 dw_4
cbx_2 cbx_2
dw_3 dw_3
st_11 st_11
st_10 st_10
cbx_validadestino cbx_validadestino
cbx_1 cbx_1
ddlb_1 ddlb_1
st_7 st_7
cbx_var cbx_var
em_fecha em_fecha
dw_11 dw_11
dw_10 dw_10
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_planilla em_planilla
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
st_6 st_6
dw_1 dw_1
dw_2 dw_2
st_8 st_8
st_9 st_9
end type
global w_gene_despacho_zeal w_gene_despacho_zeal

type variables
str_mant               istr_mant

Date		id_FechaAcceso
Time		it_HoraAcceso
Integer  ii_var, ii_productor, ii_puertoorigen, ii_puertodestino, &
			ii_destino
String   is_tipoplanilla, is_navetipotr
Long		il_navecodigo, il_PlaSag

DataWindowChild	idwc_cliente, idwc_planta, idwc_sagtipoplanilla
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public function boolean masdeundestino (integer al_planilla)
public function string clienterotulado (integer cliente)
public function integer valida_planta (integer li_planta)
public function integer valida_planta_region (integer li_planta)
public function integer valida_planta_provincia (integer li_planta)
public function integer valida_planta_comuna (integer li_planta)
public function integer valida_puerto_destino ()
public function integer valida_puerto_origen ()
public function integer valida_nave ()
public function integer valida_destino ()
public function integer valida_exportador ()
public function integer valida_agente ()
public function integer valida_especie ()
public function integer valida_variedad ()
public function integer valida_tiposenvases ()
public function integer valida_pesoneto ()
public function integer valida_reg_prv_com_detalle ()
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_destinoins
Integer		li_PldSag, li_RecZea, li_Cliente, li_Planta
String		ls_Archivo, ls_Registro, ls_Xml
Uo_Zeal		luo_Zeal

luo_Zeal = Create Uo_Zeal

dw_2.reset()

dw_2.SetTransObject(Sqlca)

ll_Numero	= 	Long(em_planilla.text)
li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[2])

sle_mensa.text	= "Rescata Información de Planilla SAG"

ll_Filas		= dw_2.Retrieve(li_Cliente, li_Planta, &
				  is_tipoplanilla, ll_Numero,ii_var,ii_productor,istr_mant.argumento[10],istr_mant.argumento[11])

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")
ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_planilla.SetFocus()
ELSE
	
	dw_2.SetSort('defe_guides')
	dw_2.Sort()

	sle_mensa.text	= "Enviando Información Via Web Service"
	
	// Web Services ZEAL
	// Habilita plantilla para Exportación DataWindow a XML
	dw_2.Modify("DataWindow.Export.XML.UseTemplate =   'zeal'")

	// Transforma XML a String
	ls_Xml = dw_2.Object.DataWindow.Data.XML
	// Asigna parametro string al objeto WS
	luo_Zeal.textXML = ls_Xml
	// Llama servicio del objeto WS
	li_RecZea =luo_Zeal.servicioWS()
	
	IF li_RecZea = 0 THEN
		
		sle_mensa.text	= "Archivo " + ls_Archivo + " Transmitido Satisfactoriamente"
		
		pb_grabar.Enabled	= False
		
		ddlb_2.SelectItem(0)
		
		UPDATE dba.despafrigoen SET
			defe_reczea	=	1
			WHERE clie_codigo = 	:li_Cliente
			AND   plde_codigo = 	:li_Planta
			AND   defe_plasag	=	:ll_Numero
			AND   defe_nturno	=	:is_tipoplanilla;
				
	ELSEIF li_RecZea = -1 THEN 			
			MessageBox("Atención", "XML, NO Se Encuentra Bien Formado.", &
					Exclamation!, Ok!)
					pb_grabar.Enabled = False
					ddlb_2.SelectItem(0)
					em_planilla.Text = ''
					em_planilla.SetFocus()
   ELSEIF li_RecZea = -2 THEN 			
			MessageBox("Atención", "XML, NO es Válido.", &
					Exclamation!, Ok!)
					pb_grabar.Enabled = False
					ddlb_2.SelectItem(0)
					em_planilla.Text = ''
					em_planilla.SetFocus()
   ELSEIF li_RecZea = -3 THEN 			
			MessageBox("Atención", "Usuario NO Válido.~nUsuario Sin Privilegio.~nUsuario o Contraseña Inválidos", &
					Exclamation!, Ok!)	
					pb_grabar.Enabled = False
					ddlb_2.SelectItem(0)
					em_planilla.Text = ''
					em_planilla.SetFocus()
   ELSEIF li_RecZea = -4 THEN 			
			MessageBox("Atención", "Error en Regla de Negocio.", &
					Exclamation!, Ok!)	
					pb_grabar.Enabled = False
					ddlb_2.SelectItem(0)
					em_planilla.Text = ''
					em_planilla.SetFocus()
   ELSEIF isnull(li_RecZea) THEN
		pb_grabar.Enabled = False
		ddlb_2.SelectItem(0)
		em_planilla.Text = ''
		em_planilla.SetFocus()
		dw_2.Reset()
		
	END IF

END IF

em_planilla.SetFocus()
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta, li_reczea
Date		ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	
	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dba.DESPAFRIGOEN
		WHERE	plde_codigo =	:li_planta
		AND	:li_codexp in (-1, clie_codigo)	
		AND	defe_plasag	=	:al_planilla
		AND	defe_nturno =  :is_tipoplanilla;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_planilla.SetFocus()
		RETURN False
	ELSEIF IsNull(ld_fecha) THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSE
		
		SELECT distinct em.puer_codigo,em.embq_ptoori,em.nave_tipotr,em.nave_codigo,en.defe_reczea, &
		em.dest_codigo,en.defe_plasag
		INTO	:ii_puertodestino,:ii_puertoorigen, :is_navetipotr,:il_navecodigo,:li_reczea, &
				:ii_destino,:il_PlaSag
		FROM	dba.DESPAFRIGOEN as en, dba.EMBARQUEPROD as em
		WHERE	en.plde_codigo =	:li_planta
		AND	:li_codexp in (-1, en.clie_codigo)	
		AND	en.defe_plasag	= :al_planilla
		AND	en.defe_nturno = :is_tipoplanilla
		AND   en.clie_codigo = em.clie_codigo
		AND   en.embq_codigo = em.embq_codigo;
		
		IF li_reczea = 1 THEN
			IF MessageBox("Atención", "Planilla Ya Fue Envíada a Zeal, Envía de Nuevo?", &
					Question!, YesNo!) = 2 THEN
					
					RETURN False
				END IF
		END IF
		
		em_fecha.text		= String(ld_fecha)
		sle_mensa.text		= ""
		
		ddlb_2.SelectItem(0)
		pb_grabar.Enabled = False
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function boolean masdeundestino (integer al_planilla);Integer	li_codexp, li_planta, li_variosdestinos

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	
	SELECT Count(distinct dest_codigo)
		INTO	:li_variosdestinos
		FROM	dba.DESPAFRIGOEN as DE, dba.PALLETENCAB as PE, dba.DESPAFRIGODE as DD
		WHERE	de.plde_codigo =	:li_planta
		AND	:li_codexp in (-1,de.clie_codigo)
		AND	de.defe_plasag	=	:al_planilla
		AND   de.clie_codigo	=	dd.clie_codigo
		AND   de.plde_codigo	=	dd.plde_codigo
		AND   de.defe_numero =  dd.defe_numero
		AND   pe.clie_codigo	=	dd.clie_codigo
		AND   pe.plde_codigo	=	dd.plde_codigo
		AND   pe.paen_numero =  dd.paen_numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSEIF li_variosdestinos > 1 THEN
					MessageBox("Atención", "Planilla S.A.G. Incluye Mas de un Destino.", &
									Exclamation!, Ok!)
					pb_grabar.Enabled	= False
					em_planilla.SetFocus()
					RETURN False
		 ELSE
					//pb_grabar.Enabled	= True
					RETURN True
		 END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function string clienterotulado (integer cliente);String	ls_rotulacion

SELECT clie_rotula
INTO   :ls_rotulacion
FROM dba.clientesprod
WHERE clie_codigo = :Cliente;

IF IsNull(ls_rotulacion) THEN
	
	ls_rotulacion	=	String(Cliente)

END IF

RETURN ls_rotulacion
end function

public function integer valida_planta (integer li_planta);Integer	li_codigo
	
SELECT plde_codsag
	INTO	:li_codigo
	FROM	dba.PLANTADESP
	WHERE	plde_codigo =	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PLANTADESP")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No Existe Código SAG para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF IsNull(li_codigo) OR li_codigo = 0 THEN
	MessageBox("Atención", "No Existe Código SAG para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()	
END IF
	
RETURN 0
end function

public function integer valida_planta_region (integer li_planta);Integer	li_codigo
	
SELECT pl.plde_region
	INTO	:li_codigo
	FROM	dba.PLANTADESP as pl
	WHERE	pl.plde_codigo =	:li_planta
	AND  exists(SELECT *
	FROM dba.REGIONES as re
	WHERE pl.plde_region=re.regi_codigo);

IF IsNull(li_codigo) THEN
	li_codigo=0
END IF

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PLANTADESP")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código REGION "+String(li_codigo)+" No Válido para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF IsNull(li_codigo) OR li_codigo = 0 THEN
	MessageBox("Atención", "Código REGION "+String(li_codigo)+" No Válido para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()	
END IF

RETURN 0
end function

public function integer valida_planta_provincia (integer li_planta);Integer	li_codigo
	
SELECT pl.plde_provin
	INTO	:li_codigo
	FROM	dba.PLANTADESP as pl
	WHERE	pl.plde_codigo =	:li_planta
	AND  exists(SELECT *
	FROM dba.PROVINCIAS as re
	WHERE pl.plde_region=re.regi_codigo
	AND   pl.plde_provin=re.prov_codigo);
				
IF IsNull(li_codigo) THEN
	li_codigo = 0
END IF
				
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PLANTADESP")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código PROVINCIA "+String(li_codigo)+" No Válido para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF IsNull(li_codigo) OR li_codigo = 0 THEN
	MessageBox("Atención", "Código PROVINCIA "+String(li_codigo)+" No Válido para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()	
END IF

RETURN 0
end function

public function integer valida_planta_comuna (integer li_planta);Integer	li_codigo
	
SELECT pl.plde_comuna
	INTO	:li_codigo
	FROM	dba.PLANTADESP as pl
	WHERE	pl.plde_codigo =	:li_planta
	AND  exists(SELECT *
	FROM dba.COMUNASEXP as re
	WHERE pl.plde_region=re.regi_codigo
	AND   pl.plde_provin=re.prov_codigo
	AND   pl.plde_comuna=re.comu_codigo);
				
IF	IsNull(li_codigo) THEN
	li_codigo	=	0
END IF

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PLANTADESP")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código COMUNA "+String(li_codigo)+" No Válido para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF IsNull(li_codigo) OR li_codigo = 0 THEN
	MessageBox("Atención", "Código COMUNA "+String(li_codigo)+" No Válido para Planta "+String(li_Planta)+" .~r~rDebe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()	
END IF

RETURN 0
end function

public function integer valida_puerto_destino ();Integer	li_codigo
String	ls_zeal

SELECT puer_codigo, puer_cdzeal
	INTO	:li_codigo, :ls_zeal
	FROM	dba.PUERTOS
	WHERE	puer_codigo =	:ii_puertodestino;
				
IF	IsNull(ls_zeal) OR ls_zeal='' THEN
	ls_zeal	=	''
END IF

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PUERTOS")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código PUERTO DESTINO "+String(li_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF (IsNull(li_codigo) OR IsNull(ls_zeal) OR ls_zeal = ''   ) THEN
	MessageBox("Atención", "Código PUERTO DESTINO "+String(li_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()	
END IF
	
RETURN 0
end function

public function integer valida_puerto_origen ();Integer	li_codigo
String	ls_zeal

SELECT puer_codigo, puer_cdzeal
	INTO	:li_codigo, :ls_zeal
	FROM	dba.PUERTOS
	WHERE	puer_codigo =	:ii_puertoorigen;

IF	IsNull(ls_zeal) OR ls_zeal='' THEN
	ls_zeal	=	''
END IF
				
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PUERTOS")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código PUERTO ORIGEN "+String(li_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF (IsNull(li_codigo) OR IsNull(ls_zeal) OR ls_zeal='' ) THEN
	MessageBox("Atención", "Código PUERTO ORIGEN "+String(li_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
END IF
	
RETURN 0
end function

public function integer valida_nave ();String	ls_zeal
Long		ll_codigo

SELECT nave_codigo, nave_cdzeal
	INTO	:ll_codigo, :ls_zeal
	FROM	dba.NAVES
	WHERE	nave_tipotr = :is_navetipotr
	AND   nave_codigo = :il_navecodigo;
				
IF	IsNull(ls_zeal) OR ls_zeal='' THEN
	ls_zeal	=	''
END IF

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla NAVES")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100  THEN
	MessageBox("Atención", "Código NAVE "+String(ll_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF (IsNull(ll_codigo) OR IsNull(ls_zeal)) OR ls_zeal='' THEN
	MessageBox("Atención", "Código NAVE "+String(ll_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
	
END IF
	
RETURN 0




end function

public function integer valida_destino ();Integer	li_codigo
String	ls_zeal

SELECT dest_codigo, dest_cdzeal
	INTO	:li_codigo, :ls_zeal
	FROM	dba.DESTINOS
	WHERE	dest_codigo =	:ii_destino;

IF	IsNull(ls_zeal) OR ls_zeal='' THEN
	ls_zeal	=	''
END IF

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESTINOS")
	em_planilla.SetFocus()
	RETURN 1
ELSEIF sqlca.SQLCode = 100  THEN
	MessageBox("Atención", "Código DESTINO "+String(li_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
ELSEIF (IsNull(li_codigo) OR IsNull(ls_zeal)) OR ls_zeal='' THEN
	MessageBox("Atención", "Código DESTINO "+String(li_codigo)+" y/o ZEAL "+ls_zeal+" .~r~r No Existe, Debe Ir a Mantención.", &
					Exclamation!, Ok!)
	RETURN 1
	em_planilla.SetFocus()
	
END IF
	
RETURN 0
end function

public function integer valida_exportador ();Integer	li_Planta, li_Cliente, li_Cliente2, li_Null
Long		ll_Cuenta, li_F, ll_Cuenta2
String	ls_Nombre, ls_Rut, ls_Null

SetNull(li_Null)
SetNull(ls_Null)

dw_1.DataObject = "dw_mues_clienprod"

dw_1.reset()

dw_1.SetTransObject(Sqlca)

//dw_1.Reset()

li_Planta	=	Integer(istr_mant.argumento[2])

ll_Cuenta	=	dw_1.Retrieve()

IF ll_Cuenta	>	0 	THEN
	
	FOR li_F=1	TO dw_1.RowCount()
		
		li_Cliente2	=	li_Null
		ls_Nombre	=	ls_Null
		ls_Rut		=	ls_Null
		
		li_Cliente	=	dw_1.Object.clie_codigo[li_F]
		
		SELECT cl.clie_codigo,cl.clie_nombre, cl.clie_nrorut
			INTO	:li_Cliente2,:ls_Nombre,:ls_Rut
			FROM	dba.CLIENTESPROD as cl
			WHERE cl.clie_codigo = :li_Cliente
			AND   exists (SELECT *
			FROM dba.DESPAFRIGOEN as de
			WHERE	plde_codigo = :li_planta
			AND	defe_plasag	= :il_plasag
			AND	defe_nturno = :is_tipoplanilla
			AND   de.clie_codigo=cl.clie_codigo);
			
			IF IsNull(li_Cliente2) OR li_Cliente2 = 0 THEN
			ELSE
					IF ( IsNull(ls_Nombre)  OR IsNull(ls_Rut) OR ls_Rut='' )THEN
						MessageBox("Atención", "No Existe Nombre o Rut para Cliente "+String(li_Cliente)+" .~r~rDebe Ir a Mantención.", &
										Exclamation!, Ok!)
						ll_Cuenta2++
					END IF			
			END IF
	NEXT
	
	IF	ll_Cuenta2 > 0 THEN
		ll_Cuenta2	=	1
	ELSE
		ll_Cuenta2	=	0
	END IF
ELSE
	ll_Cuenta2	=	1
END IF
		
RETURN ll_Cuenta2
end function

public function integer valida_agente ();Integer	li_Planta, li_Embarcador, li_Embarcador2, li_Null
Long		ll_Cuenta, li_F, ll_Cuenta2
String	ls_Nombre, ls_Rut, ls_Null

SetNull(li_Null)
SetNull(ls_Null)

dw_1.DataObject = "dw_mues_embarcadores_agente"

dw_1.reset()

dw_1.SetTransObject(Sqlca)

dw_1.Reset()

li_Planta	=	Integer(istr_mant.argumento[2])

ll_Cuenta	=	dw_1.Retrieve()

IF ll_Cuenta	>	0 	THEN
	
	FOR li_F=1	TO dw_1.RowCount()
		li_Embarcador2	=	li_Null
		ls_Nombre		=	ls_Null
		ls_Rut			=	ls_Null
		
		li_Embarcador	=	dw_1.Object.embc_codigo[li_F]
		
		SELECT cl.embc_codigo,cl.embc_ageadu, cl.embc_rutage
			INTO	:li_Embarcador2,:ls_Nombre,:ls_Rut
			FROM	dba.EMBARCADORES as cl
			WHERE cl.embc_codigo = :li_Embarcador
			AND   exists (SELECT *
			FROM dba.DESPAFRIGOEN as de, dba.EMBARQUEPROD as em
			WHERE	plde_codigo = :li_planta
			AND	defe_plasag	= :il_plasag
			AND	defe_nturno = :is_tipoplanilla
			AND   de.clie_codigo = em.clie_codigo			
			AND   de.embq_codigo = em.embq_codigo
			AND   em.embc_codigo = cl.embc_codigo);
			
			IF IsNull(li_Embarcador2) OR li_Embarcador2 = 0 THEN
			ELSE
					IF ( IsNull(ls_Nombre)  OR IsNull(ls_Rut) OR ls_Rut='' )THEN
						MessageBox("Atención", "No Existe Nombre o Rut para Agente Aduana del Embarcador "+String(li_Embarcador)+" .~r~rDebe Ir a Mantención.", &
										Exclamation!, Ok!)
						ll_Cuenta2++
					END IF			
			END IF
	NEXT
	
	IF	ll_Cuenta2 > 0 THEN
		ll_Cuenta2	=	1
	ELSE
		ll_Cuenta2	=	0
	END IF
ELSE
	ll_Cuenta2	=	1
END IF
		
RETURN ll_Cuenta2
end function

public function integer valida_especie ();Integer	li_Planta, li_Especie, li_Especie2,li_Null
Long		ll_Cuenta, li_F, ll_Cuenta2
String	ls_Nombre, ls_Sag,ls_Null

SetNull(li_Null)
SetNull(ls_Null)

dw_1.DataObject = "dw_mues_especies"

dw_1.reset()

dw_1.SetTransObject(Sqlca)

dw_1.Reset()

li_Planta	=	Integer(istr_mant.argumento[2])

ll_Cuenta	=	dw_1.Retrieve()

IF ll_Cuenta	>	0 	THEN
	
	FOR li_F=1	TO dw_1.RowCount()
		
		li_Especie2	=	li_Null
		ls_Nombre	=	ls_Null
		ls_Sag		=	ls_Null
		
		li_Especie	=	dw_1.Object.espe_codigo[li_F]
		
		SELECT cl.espe_codigo,cl.espe_nombre, cl.espe_cdzeal
			INTO	:li_Especie2,:ls_Nombre,:ls_Sag
			FROM	dba.ESPECIES as cl
			WHERE cl.espe_codigo = :li_Especie
			AND   exists (SELECT *
			FROM dba.DESPAFRIGOEN as de, dba.DESPAFRIGODE as em, dba.PALLETFRUTA as pf
			WHERE	de.plde_codigo = :li_planta
			AND	de.defe_plasag	= :il_plasag
			AND	de.defe_nturno = :is_tipoplanilla
			AND   de.clie_codigo = em.clie_codigo			
			AND   de.plde_codigo = em.plde_codigo
			AND   de.defe_numero = em.defe_numero
			AND   pf.clie_codigo = em.clie_codigo			
			AND   pf.plde_codigo = em.plde_codigo
			AND   pf.paen_numero = em.paen_numero
			AND   pf.espe_codigo = cl.espe_codigo);
			
			IF IsNull(li_Especie2) OR li_Especie2 = 0 THEN
			ELSE
					IF ( IsNull(ls_Nombre)  OR IsNull(ls_Sag) OR ls_Sag='' )THEN
						MessageBox("Atención", "No Existe Nombre o Código ZEAL para Especie "+String(li_Especie)+" .~r~rDebe Ir a Mantención.", &
										Exclamation!, Ok!)
						ll_Cuenta2++
					END IF			
			END IF
	NEXT
	
	IF	ll_Cuenta2 > 0 THEN
		ll_Cuenta2	=	1
	ELSE
		ll_Cuenta2	=	0
	END IF
ELSE
	ll_Cuenta2	=	1
END IF
		
RETURN ll_Cuenta2
end function

public function integer valida_variedad ();Integer	li_Planta, li_Especie, li_Especie2,li_Null, li_Variedad, li_Variedad2
Long		ll_Cuenta, li_F, ll_Cuenta2, ll_cuentavar,li_Fvar
String	ls_Nombre, ls_Sag,ls_Null,ls_Nombre2

SetNull(li_Null)
SetNull(ls_Null)

dw_1.DataObject = "dw_mues_especies"
dw_4.DataObject = "dw_mues_variedades"

dw_1.reset()
dw_4.reset()

dw_1.SetTransObject(Sqlca)
dw_4.SetTransObject(Sqlca)

dw_1.Reset()
dw_4.Reset()

li_Planta	=	Integer(istr_mant.argumento[2])

ll_Cuenta	=	dw_1.Retrieve()

IF ll_Cuenta	>	0 	THEN
	
	FOR li_F=1	TO dw_1.RowCount()
		
		li_Especie2	=	li_Null
		ls_Nombre	=	ls_Null
		ls_Sag		=	ls_Null
		
		li_Especie	=	dw_1.Object.espe_codigo[li_F]
		
		SELECT cl.espe_codigo,cl.espe_nombre, cl.espe_codsag
			INTO	:li_Especie2,:ls_Nombre,:ls_Sag
			FROM	dba.ESPECIES as cl
			WHERE cl.espe_codigo = :li_Especie
			AND   exists (SELECT *
			FROM dba.DESPAFRIGOEN as de, dba.DESPAFRIGODE as em, dba.PALLETFRUTA as pf
			WHERE	de.plde_codigo = :li_planta
			AND	de.defe_plasag	= :il_plasag
			AND	de.defe_nturno = :is_tipoplanilla
			AND   de.clie_codigo = em.clie_codigo			
			AND   de.plde_codigo = em.plde_codigo
			AND   de.defe_numero = em.defe_numero
			AND   pf.clie_codigo = em.clie_codigo			
			AND   pf.plde_codigo = em.plde_codigo
			AND   pf.paen_numero = em.paen_numero
			AND   pf.espe_codigo = cl.espe_codigo);
			
			IF IsNull(li_Especie2) OR li_Especie2 = 0 THEN
			ELSE
					IF ( IsNull(ls_Nombre)  OR IsNull(ls_Sag) )THEN
						MessageBox("Atención", "No Existe Nombre o Código SAG para Especie "+String(li_Especie)+" .~r~rDebe Ir a Mantención.", &
										Exclamation!, Ok!)
						ll_Cuenta2++
					ELSE
						
						ll_cuentavar = dw_4.Retrieve(li_Especie)
						
						IF ll_cuentavar	>	0 	THEN
							
							FOR li_Fvar=1	TO dw_4.RowCount()
								
								   li_Variedad2	=	li_Null
									ls_Nombre2	=	ls_Null
									
									li_Variedad	=	dw_4.Object.vari_codigo[li_Fvar]
									
									SELECT cl.vari_codigo,cl.vari_nombre
										INTO	:li_Variedad2,:ls_Nombre2
										FROM	dba.VARIEDADES as cl
										WHERE cl.espe_codigo = :li_Especie
										AND   cl.vari_codigo = :li_Variedad
										AND   exists (SELECT *
										FROM dba.DESPAFRIGOEN as de, dba.DESPAFRIGODE as em, dba.PALLETFRUTA as pf
										WHERE	de.plde_codigo = :li_planta
										AND	de.defe_plasag	= :il_plasag
										AND	de.defe_nturno = :is_tipoplanilla
										AND   de.clie_codigo = em.clie_codigo			
										AND   de.plde_codigo = em.plde_codigo
										AND   de.defe_numero = em.defe_numero
										AND   pf.clie_codigo = em.clie_codigo			
										AND   pf.plde_codigo = em.plde_codigo
										AND   pf.paen_numero = em.paen_numero
										AND   pf.espe_codigo = cl.espe_codigo
										AND   pf.vari_codigo = cl.vari_codigo);						

									IF IsNull(li_Variedad2) OR li_Variedad2 = 0 THEN
									ELSE
											IF IsNull(ls_Nombre2) THEN
												MessageBox("Atención", "No Existe Nombre para Variedad "+String(li_Especie)+' '+String(li_Variedad)+" .~r~rDebe Ir a Mantención.", &
																Exclamation!, Ok!)
												ll_Cuenta2++
											END IF
									END IF
								NEXT
							END IF
					END IF			
			END IF
	NEXT
	
	IF	ll_Cuenta2 > 0 THEN
		ll_Cuenta2	=	1
	ELSE
		ll_Cuenta2	=	0
	END IF
ELSE
	ll_Cuenta2	=	1
END IF
		
RETURN ll_Cuenta2
end function

public function integer valida_tiposenvases ();Integer	li_Planta, li_Envases, li_Envases2,li_Null
Long		ll_Cuenta, li_F, ll_Cuenta2
String	ls_Nombre, ls_Sag,ls_Null

SetNull(li_Null)
SetNull(ls_Null)

dw_1.DataObject = "dw_mues_tiposenvases"

dw_1.reset()

dw_1.SetTransObject(Sqlca)

dw_1.Reset()

li_Planta	=	Integer(istr_mant.argumento[2])

ll_Cuenta	=	dw_1.Retrieve()

IF ll_Cuenta	>	0 	THEN
	
	FOR li_F=1	TO dw_1.RowCount()
		
		li_Envases2	=	li_Null
		ls_Nombre	=	ls_Null
		ls_Sag		=	ls_Null
		
		li_Envases	=	dw_1.Object.enva_tipoen[li_F]
		
		SELECT cl.enva_tipoen,cl.tien_nombre, cl.tien_cdzeal
			INTO	:li_Envases2,:ls_Nombre,:ls_Sag
			FROM	dba.TIPOSENVASES as cl
			WHERE cl.enva_tipoen = :li_Envases
			AND   exists (SELECT *
			FROM dba.DESPAFRIGOEN as de, dba.DESPAFRIGODE as em, dba.PALLETFRUTA as pf, dba.EMBALAJESPROD as ej
			WHERE	de.plde_codigo = :li_planta
			AND	de.defe_plasag	= :il_plasag
			AND	de.defe_nturno = :is_tipoplanilla
			AND   de.clie_codigo = em.clie_codigo			
			AND   de.plde_codigo = em.plde_codigo
			AND   de.defe_numero = em.defe_numero
			AND   pf.clie_codigo = em.clie_codigo			
			AND   pf.plde_codigo = em.plde_codigo
			AND   pf.paen_numero = em.paen_numero
			AND   pf.clie_codigo = ej.clie_codigo
			AND   pf.emba_codigo = ej.emba_codigo			
			AND   ej.enva_tipoen = cl.enva_tipoen);
			
			IF IsNull(li_Envases2) OR li_Envases2 = 0 THEN
			ELSE
					IF ( IsNull(ls_Nombre)  OR IsNull(ls_Sag) OR ls_Sag='' )THEN
						MessageBox("Atención", "No Existe Nombre o Código SAG para Tipo Envase "+String(li_Envases)+" .~r~rDebe Ir a Mantención.", &
										Exclamation!, Ok!)
						ll_Cuenta2++
					END IF			
			END IF
	NEXT
	
	IF	ll_Cuenta2 > 0 THEN
		ll_Cuenta2	=	1
	ELSE
		ll_Cuenta2	=	0
	END IF
ELSE
	ll_Cuenta2	=	1
END IF
		
RETURN ll_Cuenta2
end function

public function integer valida_pesoneto ();Integer	li_Planta, li_TipoEnvase, li_TipoEnvase2,li_Null, li_Envase, li_Envase2
Long		ll_Cuenta, li_F, ll_Cuenta2, ll_cuentapeso,li_Fpeso
String	ls_Nombre, ls_Sag,ls_Null,ls_Nombre2
Decimal	ld_Pesoneto

SetNull(li_Null)
SetNull(ls_Null)

dw_1.DataObject = "dw_mues_tiposenvases"
dw_4.DataObject = "dw_mues_envases"

dw_1.reset()
dw_4.reset()

dw_1.SetTransObject(Sqlca)
dw_4.SetTransObject(Sqlca)

dw_1.Reset()
dw_4.Reset()

li_Planta	=	Integer(istr_mant.argumento[2])

ll_Cuenta	=	dw_1.Retrieve()

IF ll_Cuenta	>	0 	THEN
	
	FOR li_F=1	TO dw_1.RowCount()
		
		li_Tipoenvase2	=	li_Null
		ls_Nombre		=	ls_Null
		ls_Sag			=	ls_Null
		
		li_TipoEnvase	=	dw_1.Object.enva_tipoen[li_F]
		
		SELECT cl.enva_tipoen,cl.tien_nombre, cl.tien_cdzeal
			INTO	:li_TipoEnvase2,:ls_Nombre,:ls_Sag
			FROM	dba.TIPOSENVASES as cl
			WHERE cl.enva_tipoen = :li_TipoEnvase
			AND   exists (SELECT *
			FROM dba.DESPAFRIGOEN as de, dba.DESPAFRIGODE as em, dba.PALLETFRUTA as pf, dba.EMBALAJESPROD as ej
			WHERE	de.plde_codigo = :li_planta
			AND	de.defe_plasag	= :il_plasag
			AND	de.defe_nturno = :is_tipoplanilla
			AND   de.clie_codigo = em.clie_codigo			
			AND   de.plde_codigo = em.plde_codigo
			AND   de.defe_numero = em.defe_numero
			AND   pf.clie_codigo = em.clie_codigo			
			AND   pf.plde_codigo = em.plde_codigo
			AND   pf.paen_numero = em.paen_numero
			AND   pf.clie_codigo = ej.clie_codigo
			AND   pf.emba_codigo = ej.emba_codigo			
			AND   ej.enva_tipoen = cl.enva_tipoen);
			
			IF IsNull(li_TipoEnvase2) OR li_TipoEnvase2 = 0 THEN
			ELSE
					IF ( IsNull(ls_Nombre)  OR IsNull(ls_Sag) OR ls_Sag='' )THEN
						MessageBox("Atención", "No Existe Nombre o Código SAG para Tipo Envase "+String(li_TipoEnvase)+" .~r~rDebe Ir a Mantención.", &
										Exclamation!, Ok!)
						ll_Cuenta2++
					ELSE
						
						ll_cuentapeso = dw_4.Retrieve(li_TipoEnvase)
						
						IF ll_cuentapeso	>	0 	THEN
							
							FOR li_Fpeso=1	TO dw_4.RowCount()
								
								   li_Envase2	=	li_Null
									ls_Nombre2	=	ls_Null
									
									li_Envase	=	dw_4.Object.enva_codigo[li_Fpeso]
									
									SELECT cl.enva_codigo,cl.enva_pesone
										INTO	:li_Envase2,:ld_PesoNeto
										FROM	dba.ENVASES as cl
										WHERE cl.enva_tipoen = :li_TipoEnvase
										AND   cl.enva_codigo = :li_Envase
										AND   exists (SELECT *
										FROM dba.DESPAFRIGOEN as de, dba.DESPAFRIGODE as em, dba.PALLETFRUTA as pf, dba.EMBALAJESPROD as ej, &
											  dba.ENVASES as ev
										WHERE	de.plde_codigo = :li_planta
										AND	de.defe_plasag	= :il_plasag
										AND	de.defe_nturno = :is_tipoplanilla
										AND   de.clie_codigo = em.clie_codigo			
										AND   de.plde_codigo = em.plde_codigo
										AND   de.defe_numero = em.defe_numero
										AND   pf.clie_codigo = em.clie_codigo			
										AND   pf.plde_codigo = em.plde_codigo
										AND   pf.paen_numero = em.paen_numero
										AND   pf.clie_codigo = ej.clie_codigo
										AND   pf.emba_codigo = ej.emba_codigo			
										AND   ej.enva_tipoen = ev.enva_tipoen
										AND   ej.enva_codigo = ev.enva_codigo
										AND   ev.enva_tipoen = cl.enva_tipoen
										AND   ev.enva_codigo = cl.enva_codigo);				

									IF IsNull(li_Envase2) OR li_Envase2 = 0 THEN
									ELSE
											IF IsNull(ld_PesoNeto) OR ld_PesoNeto = 0 THEN
												MessageBox("Atención", "No Existe Peso Neto para Envase "+String(li_TipoEnvase)+' '+String(li_Envase)+" .~r~rDebe Ir a Mantención.", &
																Exclamation!, Ok!)
												ll_Cuenta2++
											END IF
									END IF
								NEXT
							END IF
					END IF			
			END IF
	NEXT
	
	IF	ll_Cuenta2 > 0 THEN
		ll_Cuenta2	=	1
	ELSE
		ll_Cuenta2	=	0
	END IF
ELSE
	ll_Cuenta2	=	1
END IF
		
RETURN ll_Cuenta2
end function

public function integer valida_reg_prv_com_detalle ();Long			ll_Filas, ll_Numero, ll_Fila, ll_Productor, ll_Predio
Integer		li_Cliente, li_Planta, li_valida

dw_5.reset()

dw_5.SetTransObject(Sqlca)

dw_5.Reset()

ll_Numero	= 	Long(em_planilla.text)
li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[2])

ll_Filas		= dw_5.Retrieve(li_Cliente, li_Planta, is_tipoplanilla, ll_Numero,ii_productor)

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación Datos de Planilla S.A.G.")
	li_valida	=	1
ELSEIF ll_Filas = 0 THEN
		li_valida	=	0
	ELSE
		li_valida	=	1
		
		FOR ll_Fila = 1 TO dw_5.RowCount()
			
			ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
			ll_Predio		=	dw_5.Object.pafr_huert1[ll_Fila]
			
			IF IsNull(ll_Productor) THEN
				ll_Productor = 0
			END IF
			IF IsNull(ll_Predio) THEN
				ll_Predio = 0
			END IF			
			

		MessageBox("Atención", "Información de Región, Provincia y/o Comuna, No Consistentes en Productor "+String(ll_Productor)+' o Predio '+String(ll_Predio)+" .~r~rDebe Ir a Mantención.",Exclamation!, Ok!)
			
		NEXT
	END IF
	
RETURN li_valida
end function

on w_gene_despacho_zeal.create
this.ddlb_2=create ddlb_2
this.dw_5=create dw_5
this.dw_4=create dw_4
this.cbx_2=create cbx_2
this.dw_3=create dw_3
this.st_11=create st_11
this.st_10=create st_10
this.cbx_validadestino=create cbx_validadestino
this.cbx_1=create cbx_1
this.ddlb_1=create ddlb_1
this.st_7=create st_7
this.cbx_var=create cbx_var
this.em_fecha=create em_fecha
this.dw_11=create dw_11
this.dw_10=create dw_10
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_planilla=create em_planilla
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.st_6=create st_6
this.dw_1=create dw_1
this.dw_2=create dw_2
this.st_8=create st_8
this.st_9=create st_9
this.Control[]={this.ddlb_2,&
this.dw_5,&
this.dw_4,&
this.cbx_2,&
this.dw_3,&
this.st_11,&
this.st_10,&
this.cbx_validadestino,&
this.cbx_1,&
this.ddlb_1,&
this.st_7,&
this.cbx_var,&
this.em_fecha,&
this.dw_11,&
this.dw_10,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_planilla,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.st_6,&
this.dw_1,&
this.dw_2,&
this.st_8,&
this.st_9}
end on

on w_gene_despacho_zeal.destroy
destroy(this.ddlb_2)
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.cbx_2)
destroy(this.dw_3)
destroy(this.st_11)
destroy(this.st_10)
destroy(this.cbx_validadestino)
destroy(this.cbx_1)
destroy(this.ddlb_1)
destroy(this.st_7)
destroy(this.cbx_var)
destroy(this.em_fecha)
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_planilla)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.st_8)
destroy(this.st_9)
end on

event open;x	=	0
y	=	0

This.Icon	=	Gstr_apl.Icono

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

//dw_3.GetChild("psag_codigo", idwc_sagtipoplanilla)
//idwc_sagtipoplanilla.SetTransObject(SQLCA)
//idwc_sagtipoplanilla.Retrieve()
//dw_3.InsertRow(0)
//dw_3.SetItem(1,"psag_codigo",1)

//istr_mant.argumento[1]	=	String(gi_codexport,'000')
istr_mant.argumento[1] = '-1'
istr_mant.argumento[2]	=	String(gi_codplanta)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
ddlb_1.SelectItem(Integer(1))
//ddlb_2.SelectItem(Integer(1))

is_tipoplanilla = '1'

IF gi_vari_rotulada = 1 THEN
	cbx_var.Checked	= True
	cbx_var.Enabled	=	False
ELSE
	cbx_var.Checked	= False
	cbx_var.Enabled	=	True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_1.Checked	=	True
	cbx_1.Enabled	=	False
ELSE
	cbx_1.Checked	= 	False
	cbx_1.Enabled	=	True
END IF

ii_var	= gi_vari_rotulada

pb_grabar.Enabled	=	False
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type ddlb_2 from dropdownlistbox within w_gene_despacho_zeal
integer x = 677
integer y = 804
integer width = 1275
integer height = 400
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"1.- INSPECCION SAG/ORIGEN(O) ","2.-Sin Condición/Sin Fumigación(F) ","3.-USDA/FUMIGADO(U)"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;
IF index	=	1 THEN
	istr_mant.argumento[10]	=	'O'
	istr_mant.argumento[11]	=	'INSPECCION SAG/ORIGEN'
ELSEIF index = 2 THEN
	istr_mant.argumento[10]	=	'F'
	istr_mant.argumento[11]	=	'Sin Condición/Sin Fumigación'
ELSE
	istr_mant.argumento[10]	=	'U'
	istr_mant.argumento[11]	=	'USDA/FUMIGADO'
END IF	

pb_grabar.Enabled			=	True
end event

type dw_5 from datawindow within w_gene_despacho_zeal
integer x = 37
integer y = 1488
integer width = 2290
integer height = 280
integer taborder = 130
string title = "none"
string dataobject = "dw_gene_despacho_zeal_reg_prv_com"
boolean livescroll = true
end type

type dw_4 from datawindow within w_gene_despacho_zeal
boolean visible = false
integer x = 123
integer y = 1260
integer width = 704
integer height = 444
integer taborder = 120
boolean bringtotop = true
string dataobject = "dw_mues_clienprod"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

type cbx_2 from checkbox within w_gene_despacho_zeal
integer x = 1838
integer y = 196
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
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-1'
	dw_10.Enabled = False	
	dw_10.Reset()
	idwc_cliente.Retrieve()
	dw_10.InsertRow(0)
ELSE
	dw_10.Enabled = True
	idwc_cliente.Retrieve()
	dw_10.InsertRow(0)
	dw_10.SetItem(1,"clie_codigo",	gi_CodExport)
	istr_mant.argumento[1] = String(gi_CodExport)
END IF	
end event

type dw_3 from datawindow within w_gene_despacho_zeal
boolean visible = false
integer y = 1732
integer width = 1157
integer height = 96
integer taborder = 90
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_sagtipoplanilla"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[10]	=	String(Integer(data),'000')

pb_grabar.Enabled	=	True
end event

type st_11 from statictext within w_gene_despacho_zeal
integer x = 133
integer y = 808
integer width = 549
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planilla Condición"
boolean focusrectangle = false
end type

type st_10 from statictext within w_gene_despacho_zeal
integer x = 46
integer y = 776
integer width = 2222
integer height = 160
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_validadestino from checkbox within w_gene_despacho_zeal
integer x = 1536
integer y = 544
integer width = 567
integer height = 80
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Valida Destino"
boolean checked = true
end type

type cbx_1 from checkbox within w_gene_despacho_zeal
integer x = 1184
integer y = 684
integer width = 695
integer height = 80
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor Rotulado "
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_productor = 1
else
	ii_productor = 0
end if	
end event

type ddlb_1 from dropdownlistbox within w_gene_despacho_zeal
integer x = 677
integer y = 440
integer width = 1275
integer height = 400
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"1. Productos Agríc. de Export. Certificados","2. Productos Agr.Export. Cert. (USDA)","3. Fruta a ser Fumigada en U.S.A.","4. Fumigados","5. Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;is_tipoplanilla	=	String(index)
end event

type st_7 from statictext within w_gene_despacho_zeal
integer x = 133
integer y = 456
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type cbx_var from checkbox within w_gene_despacho_zeal
integer x = 274
integer y = 684
integer width = 695
integer height = 80
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Variedad Rotulada "
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type em_fecha from editmask within w_gene_despacho_zeal
integer x = 1125
integer y = 536
integer width = 402
integer height = 92
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

type dw_11 from datawindow within w_gene_despacho_zeal
integer x = 677
integer y = 296
integer width = 969
integer height = 100
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	String(data)


end event

type dw_10 from datawindow within w_gene_despacho_zeal
integer x = 677
integer y = 196
integer width = 1157
integer height = 96
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(Integer(data),'000')
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type st_4 from statictext within w_gene_despacho_zeal
integer x = 133
integer y = 296
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_despacho_zeal
integer x = 133
integer y = 196
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_despacho_zeal
integer x = 64
integer y = 948
integer width = 2176
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 33543637
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_planilla from editmask within w_gene_despacho_zeal
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 677
integer y = 536
integer width = 443
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;IF Long(This.Text) <> 0 THEN
	IF ExistePlanilla(Long(This.Text)) = False THEN
		em_planilla.Text	=	''
		em_planilla.SetFocus()
	END IF
	
	IF cbx_validadestino.Checked THEN
		IF MasdeunDestino(Long(This.Text)) = False THEN
			em_planilla.Text	=	''
			em_planilla.SetFocus()
		END IF
	END IF		
END IF	
end event

type st_5 from statictext within w_gene_despacho_zeal
integer x = 37
integer y = 68
integer width = 2226
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Genera Despacho Zeal"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_despacho_zeal
integer x = 133
integer y = 552
integer width = 448
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_despacho_zeal
integer x = 46
integer y = 172
integer width = 2222
integer height = 244
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_despacho_zeal
integer x = 2350
integer y = 832
integer width = 233
integer height = 196
integer taborder = 110
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_despacho_zeal
integer x = 2354
integer y = 580
integer width = 233
integer height = 196
integer taborder = 100
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;Long	ll_Error=0

if cbx_1.checked = true then
	ii_productor = 1
else
	ii_productor = 0
end if	

sle_mensa.text	= "Validación Planta"
ll_Error	=	ll_Error + valida_planta(Integer(istr_mant.argumento[2]))
sle_mensa.text	= "Validación Región de la Planta"
ll_Error	=	ll_Error + valida_planta_Region(Integer(istr_mant.argumento[2]))
sle_mensa.text	= "Validación Provincia de la Planta"
ll_Error	=	ll_Error + valida_planta_Provincia(Integer(istr_mant.argumento[2]))
sle_mensa.text	= "Validación Comuna de la Planta"
ll_Error	=	ll_Error + valida_planta_Comuna(Integer(istr_mant.argumento[2]))
sle_mensa.text	= "Validación Puerto Origen"
ll_Error	=	ll_Error + valida_puerto_origen()
sle_mensa.text	= "Validación Puerto Destino"
ll_Error	=	ll_Error + valida_puerto_destino()
sle_mensa.text	= "Validación Nave"
ll_Error	=	ll_Error + valida_nave()
sle_mensa.text	= "Validación Destino"
ll_Error	=	ll_Error + valida_destino()
sle_mensa.text	= "Validación Exportador"
ll_Error	=	ll_Error + valida_exportador()
sle_mensa.text	= "Validación Agente de Aduanas"
ll_Error	=	ll_Error + valida_agente()
sle_mensa.text	= "Validación Especie"
ll_Error	=	ll_Error + valida_especie()
sle_mensa.text	= "Validación Variedad"
ll_Error	=	ll_Error + valida_variedad()
sle_mensa.text	= "Validación Tipo de Envase"
ll_Error	=	ll_Error + valida_tiposenvases()
sle_mensa.text	= "Validación Peso Neto"
ll_Error	=	ll_Error + valida_pesoneto()
sle_mensa.text	= "Validación Región Provincia y Comuna del Detalle"
ll_Error	=	ll_Error + valida_reg_prv_com_detalle()

IF ll_Error = 0 THEN
	Parent.TriggerEvent("ue_guardar")
ELSE
	MessageBox("Atención", "Transmisión de Datos NO Realizada.~r~rDatos Defectuosos.", &
										Exclamation!, Ok!)
	Close(Parent)
	
END IF

end event

type gb_2 from groupbox within w_gene_despacho_zeal
boolean visible = false
integer x = 2331
integer y = 528
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_1 from groupbox within w_gene_despacho_zeal
boolean visible = false
integer x = 2331
integer y = 788
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_6 from statictext within w_gene_despacho_zeal
integer x = 46
integer y = 936
integer width = 2222
integer height = 140
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_gene_despacho_zeal
boolean visible = false
integer x = 1042
integer y = 1256
integer width = 704
integer height = 444
boolean bringtotop = true
string dataobject = "dw_mues_clienprod"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

type dw_2 from datawindow within w_gene_despacho_zeal
integer x = 46
integer y = 1096
integer width = 4343
integer height = 728
boolean bringtotop = true
string dataobject = "dw_gene_despacho_zeal"
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

event clicked;//This.Print()
end event

type st_8 from statictext within w_gene_despacho_zeal
integer x = 46
integer y = 660
integer width = 2222
integer height = 116
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_gene_despacho_zeal
integer x = 46
integer y = 416
integer width = 2222
integer height = 244
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

