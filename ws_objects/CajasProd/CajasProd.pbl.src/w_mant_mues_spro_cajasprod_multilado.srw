$PBExportHeader$w_mant_mues_spro_cajasprod_multilado.srw
$PBExportComments$Lectura de Cajas con Pistola en Cuatro Códigos
forward
global type w_mant_mues_spro_cajasprod_multilado from w_mant_mues_spro_cajasprod
end type
type sle_lectura1 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
end type
type sle_lectura2 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
end type
type sle_lectura3 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
end type
type sle_lectura4 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
end type
type pb_salir2 from picturebutton within w_mant_mues_spro_cajasprod_multilado
end type
type dw_6 from datawindow within w_mant_mues_spro_cajasprod_multilado
end type
type sle_embaladora from singlelineedit within w_mant_mues_spro_cajasprod_multilado
end type
type dw_3 from datawindow within w_mant_mues_spro_cajasprod_multilado
end type
type gb_7 from groupbox within w_mant_mues_spro_cajasprod_multilado
end type
type pb_5 from picturebutton within w_mant_mues_spro_cajasprod_multilado
end type
type gb_6 from groupbox within w_mant_mues_spro_cajasprod_multilado
end type
end forward

global type w_mant_mues_spro_cajasprod_multilado from w_mant_mues_spro_cajasprod
integer width = 4722
integer height = 2852
string title = "CAJAS EN PRODUCCION"
boolean controlmenu = true
windowtype windowtype = popup!
windowstate windowstate = maximized!
long il_lotemenor = 48759828
sle_lectura1 sle_lectura1
sle_lectura2 sle_lectura2
sle_lectura3 sle_lectura3
sle_lectura4 sle_lectura4
pb_salir2 pb_salir2
dw_6 dw_6
sle_embaladora sle_embaladora
dw_3 dw_3
gb_7 gb_7
pb_5 pb_5
gb_6 gb_6
end type
global w_mant_mues_spro_cajasprod_multilado w_mant_mues_spro_cajasprod_multilado

type variables
uo_entidades			iuo_entidad

Long						il_embaladora, il_seccontrol
Integer					ii_controllinea
String					is_LadoSalida, is_lados[3]	=	{'A', 'B', 'C'}

end variables

forward prototypes
public function long cargalote (integer tipo, long numero, integer cliente, integer planta)
public subroutine keycontrol (keycode ak_key)
protected function boolean wf_actualiza_db ()
public function boolean cargaparametroslinea ()
public function boolean despiececodcorto (string as_registro)
public function boolean iscorrelvalido (integer ai_cliente, integer ai_planta, long al_embaladora, long al_correlativo)
public function boolean cargaprograma ()
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public function boolean cargaparametros ()
end prototypes

public function long cargalote (integer tipo, long numero, integer cliente, integer planta);Long	ll_lote

Select	Max(lote_codigo)
	into 	:ll_lote
	from 	dba.spro_ordenprocdeta
	where orpr_tipord	=	:tipo
	  and orpr_numero =	:numero
	  and clie_codigo	=	:cliente
	  and plde_codigo	=	:planta;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	Return -1
ELSEIF sqlca.SQLCode	= 100 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	Return -1
ELSE
	Return ll_lote
END IF
end function

public subroutine keycontrol (keycode ak_key);String 	ls_calibre, ls_embalaje

ls_calibre	=	"NoRead"
ls_embalaje	=	"NoRead"

IF ak_key = keyQ! OR ak_key = key1! THEN
	IF dw_embala.RowCount() >= 1 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[1]
		ls_embalaje	=	dw_embala.Object.emba_codigo[1]
		dw_embala.SelectRow(1, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyP! OR ak_key = key2! THEN
	IF dw_embala.RowCount() >= 2 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[2]
		ls_embalaje	=	dw_embala.Object.emba_codigo[2]
		dw_embala.SelectRow(2, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyZ! OR ak_key = key3! THEN
	IF dw_embala.RowCount() >= 3 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[3]
		ls_embalaje	=	dw_embala.Object.emba_codigo[3]
		dw_embala.SelectRow(3, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyM! OR ak_key = key4! THEN
	IF dw_embala.RowCount() >= 4 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[4]
		ls_embalaje	=	dw_embala.Object.emba_codigo[4]
		dw_embala.SelectRow(4, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keySpaceBar! OR ak_key = key5! THEN
	IF dw_embala.RowCount() >= 5 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[5]
		ls_embalaje	=	dw_embala.Object.emba_codigo[5]
		dw_embala.SelectRow(4, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad5! OR ak_key = key5! THEN
	IF dw_embala.RowCount() >= 5 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[5]
		ls_embalaje	=	dw_embala.Object.emba_codigo[5]
		dw_embala.SelectRow(5, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad6! OR ak_key = key6! THEN
	IF dw_embala.RowCount() >= 6 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[6]
		ls_embalaje	=	dw_embala.Object.emba_codigo[6]
		dw_embala.SelectRow(6, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad7! OR ak_key = key7! THEN
	IF dw_embala.RowCount() >= 7 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[7]
		ls_embalaje	=	dw_embala.Object.emba_codigo[7]
		dw_embala.SelectRow(7, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad8! OR ak_key = key8! THEN
	IF dw_embala.RowCount() >= 8 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[8]
		ls_embalaje	=	dw_embala.Object.emba_codigo[8]
		dw_embala.SelectRow(8, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad9! OR ak_key = key9! THEN
	IF dw_embala.RowCount() >= 9 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[9]
		ls_embalaje	=	dw_embala.Object.emba_codigo[9]
		dw_embala.SelectRow(9, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF NOT ib_respuesta THEN 
	pb_ok.TriggerEvent(Clicked!)
END IF

IF ls_embalaje <> "NoRead" THEN
	sle_lectura3.Text	=	"03" + ls_embalaje
	sle_lectura4.Text	=	"04" + ls_calibre
	
	pb_ok.TriggerEvent(Clicked!)
	
	sle_lectura1.text	=	"01" + String(ii_proceso)
	sle_lectura2.text	=	"02" + String(il_lotemenor)
END IF
end subroutine

protected function boolean wf_actualiza_db ();IF dw_1.update() = 1 THEN 
	Commit;
	IF sqlca.sqlcode <> 0 THEN
		F_ErrorBaseDatos(sqlca,This.title)
		Return False
	ELSE
		Return true
	END IF
ELSE
	MessageBox("Error", "El Compacto no podra ser emitido por protección de la base de datos, ~r~n"+&
							  "favor de comunicarse con el encargado de Packing")
	Rollback;
	IF sqlca.sqlcode <> 0 THEN 
		F_ErrorBaseDatos(sqlca,this.title)
		Return false
	END IF
	Return false
END IF

Return True
end function

public function boolean cargaparametroslinea ();Integer	li_control

il_salidatrans	=	Integer(Left(is_LadoSalida, 2))

select Max(equi_nombre), Min(line_codigo), count(*)
  into :is_Computador, :ii_lineatrans, :li_control
  from dba.spro_correlcompequipo
 where loco_comlin = :il_salidatrans
   and plde_codigo =	:ii_Planta;
	

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de parametros desde spro_correlcompequipo")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 OR li_control = 0 OR IsNull(li_control) THEN
	MessageBox("Error","La salida ingresada no se encuentra habilitado para la emision de compactos.")
	RETURN FALSE
ELSEIF li_control > 1 THEN
	MessageBox("Error","La salida ingresada esta programada con mas de un codigo para esta planta.")
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean despiececodcorto (string as_registro);String	ls_lado
Integer	li_salida
Long		ll_correl, ll_filas

IF IsNumber(Left(as_registro, 2)) THEN
	li_salida	=	Integer(Left(as_registro, 2))
ElSE
	MessageBox("Error", "Código de Salida no es valido.~r~nIngrese o Seleccione Otro")
	RETURN False
END IF

FOR ll_filas = 1 TO 3
	IF Mid(as_registro, 3, 1) = is_lados[ll_filas] THEN
		ls_lado	=	is_lados[ll_filas]
	END IF
NEXT

IF ls_lado = "" OR IsNull(ls_lado) THEN
	MessageBox("Error", "Código de Lado no es valido.~r~nIngrese o Seleccione Otro")
	RETURN False
END IF

IF IsNumber(Right(as_registro, Len(as_registro) - 3)) THEN
	ll_correl	=	Long(Right(as_registro, Len(as_registro) - 3))
ElSE
	MessageBox("Error", "Código de Correlativo no es valido.~r~nIngrese o Seleccione Otro")
	RETURN False
END IF


is_LadoSalida	=	String(li_salida, '00') + ls_lado
il_seccontrol	=	ll_correl

Return True
end function

public function boolean iscorrelvalido (integer ai_cliente, integer ai_planta, long al_embaladora, long al_correlativo);Decimal	ld_resultado
Date		ld_fecha

ld_fecha			=	Date( Today() )
al_embaladora	=	-1

DECLARE valida PROCEDURE FOR dba.fgran_validacorrelativo  
	@Cliente 		= :ai_cliente,   
	@Planta 			= :ai_planta,   
	@Fecha			= :ld_fecha,
	@Embaladora 	= :al_embaladora,   
	@correlativo	= :al_correlativo
	Using sqlca;

Execute valida;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
									"fgran_validacorrelativo" )
	Close valida;
	Return False
ELSE
	Fetch valida into :ld_resultado;
	Close valida;
	IF ld_resultado < 1 THEN
		Return True
	ELSE
		MessageBox("Protección de Datos", "La caja esta asociada a un proceso con " + &
													 "Cierre Web.~r~nComuniquese con Encargado.", StopSign!)
		Return False
	END IF
END IF
end function

public function boolean cargaprograma ();Integer	li_tipo, li_cliente, li_agrupa, li_salidaconso, li_conso, li_lineapack, li_secuen
Long		ll_correl, ll_filas, ll_numero


SELECT	loco_comlin, loco_nropal
	INTO	:li_lineapack, :il_loco_nropal
	FROM	dba.spro_correlcompequipo
	WHERE	plde_codigo 			= 	:gstr_paramplanta.codigoplanta	
	AND 	line_codigo 			=	:ii_linea
	AND 	Upper(equi_nombre)	= 	Upper(:is_Computador);

	IF isNull(ii_lineapack) OR ii_lineapack = 0 THEN ii_lineapack = li_lineapack

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de correlativos de compactos: spro_correlcompequipo")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 THEN
	MessageBox("Error","No existe correlativos de Compactos para el Presente Equipo: " + is_computador)
	RETURN FALSE
END IF

IF ii_lineatrans = 0 THEN ii_lineatrans 	= 	ii_linea 
IF il_salidatrans= 0 THEN il_salidatrans	= 	ii_salida 

SELECT	orpr_tipord, orpr_numero, clie_codigo, prsa_lineaa
	INTO	:li_tipo, :ll_numero, :li_cliente, :li_secuen
	FROM	dba.spro_programasalidas 
	WHERE	prsa_estado	= 	1
	AND	plde_codigo = 	:gstr_paramplanta.codigoplanta
	AND 	prsa_lineaa	=	:ii_lineatrans;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	RETURN FALSE
END IF	

ii_cliente2 = li_cliente

SELECT	DISTINCT psrd_consol, psrd_lincon, cate_codigo
	INTO	:li_conso, :li_salidaconso, :ii_categoria
	FROM	dba.spro_programasalidadeta psd
	WHERE	( psd.plde_codigo 	= 	:gstr_paramplanta.codigoplanta ) 		and  
			( psd.orpr_tipord 	= 	:li_tipo 		)	and  
			( psd.orpr_numero 	= 	:ll_numero 		) 	and  
			( psd.clie_codigo 	= 	:li_cliente 	) 	and  
			( psd.lisa_codigo 	= 	:il_salidatrans)	and
			( psd.line_codigo		=	:ii_lineatrans )	and
			( psd.prsd_codsec		=	:is_LadoSalida )	and
			( psd.psrd_lincon		=	:ii_salida)			;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 AND ii_salida <> il_salidatrans THEN
	MessageBox("Error","La salida indicada en el adhesivo no corresponde ~r~n" + &
							 "a la salida actual o a alguna salida consolidada.")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	RETURN FALSE
END IF

IF li_conso = 1 THEN ii_lineapack = li_salidaconso

ii_proceso		=	ll_numero
il_lotemenor	=	LoteProceso(li_tipo, ll_numero, li_cliente, gstr_paramplanta.codigoplanta)

IF il_lotemenor = -1 THEN
	MessageBox("Error","No existen Lotes para el Proceso Activo.")
	RETURN FALSE
END IF

SELECT	Count(DISTINCT emba_codigo)
	INTO	:li_agrupa
	FROM	dba.spro_programasalidadeta psd
	WHERE	( psd.plde_codigo 		= 	:gstr_paramplanta.codigoplanta 	) 		and  
					( psd.orpr_tipord = 	:li_tipo 								) 		and  
					( psd.orpr_numero = 	:ll_numero 								) 		and  
					( psd.clie_codigo = 	:li_cliente 							) 		and  
					( psd.lisa_codigo = 	:il_salidatrans 						)		and
					( psd.line_codigo	=	:ii_lineatrans 						)		and
					( psd.prsd_codsec	=	:is_LadoSalida 						)		;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	RETURN FALSE
ELSEIF li_agrupa > 1 THEN
	ib_MultiEmbalaje	=	True
	ll_filas	=	dw_embala.Retrieve(gstr_paramplanta.codigoplanta, li_tipo, ll_numero, li_cliente, ii_linea, ii_lineapack )
	IF ll_filas < 1 AND ii_lineatrans <> 0 THEN
		MessageBox("Advertencia", "No Existen Embalajes para esta Salida en la Programación Activa.")
		RETURN FALSE
	END IF
ELSE
	ib_MultiEmbalaje	=	FALSE
	SELECT	psd.emba_codigo, tpem_cancaj
		INTO	:is_embala, :ii_totalpallet
		FROM	dba.spro_programasalidadeta psd,
					dba.tipopallemba tpe
		WHERE	( psd.clie_codigo	=	tpe.clie_codigo )
		AND 	( psd.emba_codigo	=	tpe.emba_codigo )
		AND	( psd.tpem_codigo	=	tpe.tpem_codigo )
		AND	( psd.plde_codigo = 	:gstr_paramplanta.codigoplanta )
		AND	( psd.orpr_tipord = 	:li_tipo ) 
		AND	( psd.orpr_numero = 	:ll_numero )
		AND	( psd.clie_codigo = 	:li_cliente )
		AND 	( psd.lisa_codigo = 	:il_salidatrans )
		AND	( psd.line_codigo	=	:ii_lineatrans )
		AND	( psd.prsd_codsec	=	:is_LadoSalida );		
END IF

SELECT Count(DISTINCT psd.prsd_calibr)
		INTO :li_agrupa
		FROM dba.spro_programasalidadeta psd
	WHERE	( psd.plde_codigo	= 	:gstr_paramplanta.codigoplanta )
	  AND ( psd.orpr_tipord = 	:li_tipo )
	  AND ( psd.orpr_numero = 	:ll_numero )
	  AND ( psd.clie_codigo = 	:li_cliente )
	  AND	( psd.lisa_codigo = 	:il_salidatrans )
	  AND ( psd.line_codigo	=	:ii_lineatrans )
	  AND ( psd.prsd_codsec	=	:is_LadoSalida );
IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	RETURN FALSE
ELSEIF li_agrupa > 1 THEN
	ib_MultiCalibre	=	True
	ll_filas	=	dw_calibr.Retrieve(gstr_paramplanta.codigoplanta, li_tipo, ll_numero, li_cliente, ii_linea, ii_lineapack )
	IF ll_filas < 2 AND ii_lineatrans <> 0 THEN
		MessageBox("Advertencia", "No Existen Calibres para esta Salida en la Programación Activa.")
		RETURN FALSE
	END IF
ELSE
	ib_MultiCalibre	=	FALSE
	SELECT psd.prsd_calibr
			INTO :is_calib
			FROM dba.spro_programasalidadeta psd
			WHERE	( psd.plde_codigo = 	:gstr_paramplanta.codigoplanta )
			AND	( psd.orpr_tipord = 	:li_tipo )
			AND	( psd.orpr_numero = 	:ll_numero )
			AND	( psd.clie_codigo = 	:li_cliente )
			AND 	( psd.lisa_codigo = 	:il_salidatrans )
			AND	( psd.line_codigo	=	:ii_lineatrans )
		   AND 	( psd.prsd_codsec	=	:is_LadoSalida );
	IF LEN ( is_calib ) < 1 AND ii_lineatrans <> 0 THEN
		MessageBox("Advertencia", "No Existen Calibres para esta Salida en la Programación Activa.")
		RETURN FALSE
	END IF
END IF

RETURN TRUE
end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);IF il_NroCaja < 1 OR IsNull(il_NroCaja) THEN

	IF NOT iuo_correl.Existe(ii_Planta,99, is_Computador, TRUE, sqlca) THEN
		SetNull(il_NroCaja)
		RETURN FALSE
	ELSE
		il_NroCaja			=	iuo_correl.il_correcompa
		dw_2.DataObject	=	iuo_correl.loco_dwcomp
		dw_2.SetTransObject(sqlca)
	END IF

ELSE
	il_NroCaja = il_NroCaja + 1
END IF

dw_1.Object.capr_numero[1]	=	il_NroCaja

RETURN True
end function

public function boolean cargaparametros ();Integer	li_control, li_salida

ii_controllinea	++

IF ii_controllinea = 1 THEN
	RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
					"ComputerName", RegString!, is_Computador)
END IF

SELECT Min(line_codigo), Min(loco_comlin), count(*)
  INTO :ii_linea, :il_salidatrans, :li_control
  FROM dba.spro_correlcompequipo
 WHERE equi_nombre = :is_Computador
   AND plde_codigo =	:ii_Planta;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de parametros desde spro_correlcompequipo")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 OR li_control = 0 OR IsNull(li_control) THEN
	MessageBox("Error","El computador no se encuentra habilitado para la emision de compactos.")
	RETURN FALSE
ELSEIF li_control > 1 THEN
	MessageBox("Error","Existe más de un computador habilitado con el mismo nombre para esta planta.")
	RETURN FALSE
ELSE
	IF Len(is_LadoSalida) > 0 THEN
		li_salida	=	Integer(Left(is_LadoSalida, 2))
		IF li_salida <> il_salidatrans THEN
			IF NOT cargaparametroslinea() THEN Return False
			IF NOT CargaParametros() THEN Return False
			SetNull(il_NroCaja)
			IF NOT BuscaNuevoCorrelativo(ii_Planta,ii_Cliente) THEN
				HALT
			END IF
		ELSE
			IF ii_controllinea = 1 THEN
				ii_salida	=	il_salidatrans
				SetNull(il_NroCaja)
				IF NOT BuscaNuevoCorrelativo(ii_Planta,ii_Cliente) THEN
					HALT
				END IF
			END IF
		END IF
	ELSE
		ii_salida	=	il_salidatrans
		SetNull(il_NroCaja)
		IF NOT BuscaNuevoCorrelativo(ii_Planta,ii_Cliente) THEN
			HALT
		END IF
	END IF
END IF

RETURN TRUE
end function

on w_mant_mues_spro_cajasprod_multilado.create
int iCurrent
call super::create
this.sle_lectura1=create sle_lectura1
this.sle_lectura2=create sle_lectura2
this.sle_lectura3=create sle_lectura3
this.sle_lectura4=create sle_lectura4
this.pb_salir2=create pb_salir2
this.dw_6=create dw_6
this.sle_embaladora=create sle_embaladora
this.dw_3=create dw_3
this.gb_7=create gb_7
this.pb_5=create pb_5
this.gb_6=create gb_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_lectura1
this.Control[iCurrent+2]=this.sle_lectura2
this.Control[iCurrent+3]=this.sle_lectura3
this.Control[iCurrent+4]=this.sle_lectura4
this.Control[iCurrent+5]=this.pb_salir2
this.Control[iCurrent+6]=this.dw_6
this.Control[iCurrent+7]=this.sle_embaladora
this.Control[iCurrent+8]=this.dw_3
this.Control[iCurrent+9]=this.gb_7
this.Control[iCurrent+10]=this.pb_5
this.Control[iCurrent+11]=this.gb_6
end on

on w_mant_mues_spro_cajasprod_multilado.destroy
call super::destroy
destroy(this.sle_lectura1)
destroy(this.sle_lectura2)
destroy(this.sle_lectura3)
destroy(this.sle_lectura4)
destroy(this.pb_salir2)
destroy(this.dw_6)
destroy(this.sle_embaladora)
destroy(this.dw_3)
destroy(this.gb_7)
destroy(this.pb_5)
destroy(this.gb_6)
end on

event open;call super::open;iuo_entidad		=	Create uo_entidades

dw_embala.SetTRansObject(sqlca)
dw_calibr.SetTRansObject(sqlca)

IF ib_respuesta THEN
	sle_lectura1.text	=	"01" + String(ii_proceso)
	sle_lectura2.text	=	"02" + String(il_lotemenor)
	
	IF NOT ib_MultiEmbalaje THEN
		sle_lectura3.Text		=	"03" + is_embala
	END IF	

	IF NOT ib_MultiCalibre THEN
		sle_lectura4.Text		=	"04" + is_calib
	END IF
	
ELSE

	sle_lectura1.enabled		=	TRUE
	sle_lectura2.enabled		=	TRUE
	sle_lectura3.enabled		=	TRUE
	sle_lectura4.enabled		=	TRUE
END IF

dw_1.ShareData(dw_3)
dw_embala.ShareData(dw_6)

gb_2.Visible				=	False
gb_4.Visible				=	False
gb_5.Visible				=	False
st_1.Visible				=	False

dw_1.Visible				=	False
dw_3.visible				=	True
dw_6.Visible				=	False
dw_embala.Visible			=	False

sle_lectura1.Visible 	= 	False
sle_lectura2.Visible 	= 	False
sle_lectura3.Visible 	= 	False
sle_lectura4.Visible 	= 	False
st_advertencia.Visible	=	False

pb_ok.Visible				=	False
pb_salir.Visible			=	False
pb_salir2.Visible			=	True

sle_embaladora.Visible	=	True
sle_embaladora.SetFocus()
This.Height					=	1640

SetNull(il_embaladora)
end event

event ue_imprimir;SetPointer(HourGlass!)
Long		ll_fila

ll_Fila 	= 	dw_2.Retrieve(dw_1.Object.clie_codigo[1], &
								  dw_1.Object.plde_codigo[1], &
								  dw_1.Object.capr_numero[1], &
								  dw_1.Object.capr_numero[1],1)
			
dw_2.Print(False, False)

SetPointer(Arrow!)
end event

event key;call super::key;IF ib_MultiCalibre OR ib_MultiEmbalaje THEN

END IF
end event

event resize;call super::resize;Long 			ll_screenWidth,ll_screenHeight
environment lenv_display
long 			ll_start, ll_used, ll_height

IF NOT ib_MultiCalibre AND NOT ib_MultiEmbalaje THEN
	IF GetEnvironment(lenv_display) = 1 THEN
		ll_screenWidth 			= 	PixelsToUnits(lenv_display.screenwidth,	XPixelsToUnits!)
		ll_screenHeight 			= 	PixelsToUnits(lenv_display.screenheight,	YPixelsToUnits!)
		
		IF lenv_display.screenheight = 600 THEN
			gb_6.Visible			=	True
			pb_5.Visible			=	True
			pb_salir2.Visible		=	False
			gb_7.Visible			=	False
		ELSE
			gb_7.Visible			=	True
			pb_salir2.Visible		=	True
			pb_5.Visible			=	False
			gb_6.Visible			=	False
		END IF
		
		ll_height					=	( ll_screenHeight - ( dw_3.height + sle_embaladora.height ) ) / 2
		
		dw_3.x 						= 	(ll_screenWidth - dw_3.width) 			/ 2
		dw_3.y 						=	(ll_Height)
		
		sle_embaladora.x 			= 	(ll_screenWidth - sle_embaladora.width) 			/ 2
		sle_embaladora.y		 	=	(ll_Height	+	dw_3.height)
	END IF
END IF
end event

type sle_lectura from w_mant_mues_spro_cajasprod`sle_lectura within w_mant_mues_spro_cajasprod_multilado
boolean visible = false
integer x = 101
integer width = 2560
integer taborder = 0
end type

type pb_grafico from w_mant_mues_spro_cajasprod`pb_grafico within w_mant_mues_spro_cajasprod_multilado
integer x = 3337
integer y = 1788
end type

type st_advertencia from w_mant_mues_spro_cajasprod`st_advertencia within w_mant_mues_spro_cajasprod_multilado
end type

type dw_5 from w_mant_mues_spro_cajasprod`dw_5 within w_mant_mues_spro_cajasprod_multilado
integer x = 64
integer y = 2552
end type

type dw_1 from w_mant_mues_spro_cajasprod`dw_1 within w_mant_mues_spro_cajasprod_multilado
end type

event dw_1::sqlpreview;//
end event

type pb_ok from w_mant_mues_spro_cajasprod`pb_ok within w_mant_mues_spro_cajasprod_multilado
integer taborder = 50
boolean map3dcolors = true
end type

event pb_ok::clicked;String	ls_Registro, ls_Null, ls_ParaDespiece
Integer	li_Etiqueta

SetNull(ls_Null)
SetPointer(HourGlass!)

IF NOT ib_MultiEmbalaje AND NOT ib_MultiCalibre THEN	//---------------------
	IF NOT IsCorrelValido(ii_cliente2, ii_planta, il_embaladora, il_seccontrol) THEN Return
END IF																//---------------------

IF IsNull(sle_lectura1.Text) OR sle_lectura1.Text = '' THEN
	sle_lectura1.SetFocus()
	RETURN
END IF

IF IsNull(sle_lectura2.Text) OR sle_lectura2.Text = '' THEN
	sle_lectura2.SetFocus()
	RETURN
END IF

IF IsNull(sle_lectura3.Text) OR sle_lectura3.Text = '' THEN
	sle_lectura3.SetFocus()
	RETURN
END IF

IF IsNull(sle_lectura4.Text) OR sle_lectura4.Text = '' THEN
	sle_lectura4.SetFocus()
	RETURN
END IF

//IF NOT validaprocedimiento() THEN
//	CargaPrograma()
//END IF

ls_ParaDespiece	=	"&"+sle_lectura1.Text+"&"+sle_lectura2.Text+"&"+sle_lectura3.Text+"&"+sle_lectura4.Text

IF IsNull(ls_ParaDespiece) OR &
	ls_ParaDespiece = "" OR &
	ls_ParaDespiece = "-1" THEN	
	ls_ParaDespiece = ls_Null
	sle_lectura1.SetFocus()
ELSE
	ls_Registro	=	ls_ParaDespiece

	dw_1.Reset()
	dw_1.InsertRow(0)

	IF DespieceRegistro(ls_Registro) THEN
		IF BuscaDatosProceso(ii_proceso) THEN	
		
			PalletActual()
			
			dw_1.Object.capr_numero[1]	=	il_NroCaja
			dw_1.Object.capr_numpal[1]	=	il_loco_nropal
			dw_1.Object.capr_regcap[1]	=	ls_ParaDespiece
			dw_1.Object.capr_pcline[1]	=	is_Computador
			dw_1.Object.capr_numgia[1]	=	ii_totalpallet
			dw_1.Object.capr_lineas[1]	=	il_salidatrans
			dw_1.Object.capr_tipdoc[1]	=	4
			
			IF ib_MultiEmbalaje OR ib_MultiCalibre THEN	//---------------------
				SetNull(il_seccontrol)							//---------------------
			END IF													//---------------------

			dw_1.Object.cpco_numero[1] =	il_seccontrol

			IF NOT IsNull(il_embaladora) THEN
				dw_1.Object.capr_embala[1]	=	il_embaladora
			END IF
			
			il_NroCaja++
			
			IF IsNull(iuo_embalajesprod.CodEtiq) THEN 
				li_Etiqueta	=  1
			ELSE
				li_Etiqueta	=	iuo_embalajesprod.CodEtiq
			END IF
	
			dw_1.Object.etiq_codigo[1]		=	li_Etiqueta
		
			IF gstr_paramplanta.GenPucho 	= 1 THEN	dw_1.Object.capr_estado[1]	=	1
			IF dw_1.Object.clie_codigo[1] < 100 THEN
				dw_1.Object.capr_numtra[1]		=	dw_1.Object.clie_codigo[1]  *1000000 + &
															dw_1.Object.plde_codigo[1]  *10000 + &
															dw_1.Object.capr_docrel[1]
			ELSE
				dw_1.Object.capr_numtra[1]		=	dw_1.Object.clie_codigo[1]  *100000 + &
															dw_1.Object.plde_codigo[1]  *1000 + &
															dw_1.Object.capr_docrel[1]
			END IF
			
			dw_1.AcceptText()
		
			Parent.TriggerEvent("ue_guardar")
			IF Message.DoubleParm <> -1 THEN
				Parent.TriggerEvent("ue_imprimir")
			END IF
		ELSE
			Parent.TriggerEvent("ue_imprimirerror")
		END IF
	ELSE
		Parent.TriggerEvent("ue_imprimirerror")
   END IF

	sle_lectura1.Text = ls_Null
	sle_lectura2.Text = ls_Null
	sle_lectura3.Text = ls_Null
	sle_lectura4.Text = ls_Null
	IF ib_respuesta THEN
		sle_lectura1.text			=	"01" + String(ii_proceso)
		sle_lectura2.text			=	"02" + String(il_lotemenor)
		
		IF NOT ib_MultiEmbalaje THEN
			sle_lectura3.Text		=	"03" + is_embala
		END IF	
		
		IF NOT ib_MultiCalibre THEN
			sle_lectura4.Text		=	"04" + is_calib
		END IF
	ELSE
		sle_lectura1.enabled		=	TRUE
		sle_lectura2.enabled		=	TRUE
		sle_lectura3.enabled		=	TRUE
		sle_lectura4.enabled		=	TRUE
	END IF
	
	IF sle_lectura3.Enabled THEN
		sle_lectura3.SetFocus()
	ELSEIF sle_lectura4.Enabled THEN
		sle_lectura4.SetFocus()
	ELSE
		THIS.SetFocus()
	END IF
END IF

ib_Bloqueo	=	FALSE
dw_embala.SelectRow(0, False)

IF sle_embaladora.Visible THEN
	sle_embaladora.Text		=	''
	SetNull(il_embaladora)
	sle_embaladora.SetFocus()
END IF
end event

type pb_salir from w_mant_mues_spro_cajasprod`pb_salir within w_mant_mues_spro_cajasprod_multilado
integer y = 844
integer taborder = 60
end type

type gb_1 from w_mant_mues_spro_cajasprod`gb_1 within w_mant_mues_spro_cajasprod_multilado
integer x = 3291
integer y = 1712
end type

type gb_2 from w_mant_mues_spro_cajasprod`gb_2 within w_mant_mues_spro_cajasprod_multilado
integer y = 448
end type

type gb_4 from w_mant_mues_spro_cajasprod`gb_4 within w_mant_mues_spro_cajasprod_multilado
integer y = 768
end type

type gb_5 from w_mant_mues_spro_cajasprod`gb_5 within w_mant_mues_spro_cajasprod_multilado
integer width = 2619
end type

type st_1 from w_mant_mues_spro_cajasprod`st_1 within w_mant_mues_spro_cajasprod_multilado
end type

type dw_embala from w_mant_mues_spro_cajasprod`dw_embala within w_mant_mues_spro_cajasprod_multilado
boolean visible = false
integer x = 32
integer y = 1048
integer width = 1591
integer height = 1048
boolean titlebar = true
string title = "Programaciones Maquina"
end type

type dw_calibr from w_mant_mues_spro_cajasprod`dw_calibr within w_mant_mues_spro_cajasprod_multilado
boolean visible = false
end type

type rb_l1 from w_mant_mues_spro_cajasprod`rb_l1 within w_mant_mues_spro_cajasprod_multilado
integer x = 809
integer y = 2848
end type

type rb_l2 from w_mant_mues_spro_cajasprod`rb_l2 within w_mant_mues_spro_cajasprod_multilado
integer x = 809
integer y = 2848
end type

type gb_3 from w_mant_mues_spro_cajasprod`gb_3 within w_mant_mues_spro_cajasprod_multilado
integer x = 731
integer y = 2620
integer taborder = 70
end type

type dw_4 from w_mant_mues_spro_cajasprod`dw_4 within w_mant_mues_spro_cajasprod_multilado
integer x = 64
integer y = 2540
end type

type dw_7 from w_mant_mues_spro_cajasprod`dw_7 within w_mant_mues_spro_cajasprod_multilado
integer x = 1047
integer y = 2844
end type

type dw_2 from w_mant_mues_spro_cajasprod`dw_2 within w_mant_mues_spro_cajasprod_multilado
integer x = 1966
integer y = 2764
string dataobject = "dw_info_spro_cajasprod"
end type

type sle_lectura1 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
integer x = 101
integer y = 236
integer width = 631
integer height = 136
integer taborder = 10
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type sle_lectura2 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
integer x = 745
integer y = 236
integer width = 631
integer height = 136
integer taborder = 20
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type sle_lectura3 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
integer x = 1390
integer y = 236
integer width = 631
integer height = 136
integer taborder = 30
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_embalaje
Integer	li_fila

ls_embalaje	=	Right(This.Text, len(This.Text) - 2)

IF dw_embala.RowCount() < 1 THEN Return

li_fila	=	dw_embala.Find("emba_codigo = '" + ls_embalaje + "'", 1, dw_embala.RowCount())

IF li_fila < 1 THEN
	THIS.Text = ""
	THIS.SetFocus()
ELSE
	ii_totalpallet	=	dw_embala.Object.tpem_cancaj[li_fila]
END IF
end event

type sle_lectura4 from singlelineedit within w_mant_mues_spro_cajasprod_multilado
integer x = 2034
integer y = 236
integer width = 631
integer height = 136
integer taborder = 40
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_calibre
Integer	li_fila

ls_calibre	=	Right(This.Text, len(This.Text) - 2)

IF dw_calibr.RowCount() < 1 THEN Return

li_fila	=	dw_calibr.Find("prsd_calibr = '" + ls_calibre + "'", 1, dw_calibr.RowCount())

IF li_fila < 1 THEN
	THIS.Text = ""
	THIS.SetFocus()
END IF
end event

type pb_salir2 from picturebutton within w_mant_mues_spro_cajasprod_multilado
boolean visible = false
integer x = 4357
integer y = 2632
integer width = 155
integer height = 132
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type dw_6 from datawindow within w_mant_mues_spro_cajasprod_multilado
event key pbm_dwnkey
boolean visible = false
integer x = 576
integer y = 1044
integer width = 2459
integer height = 1076
integer taborder = 70
boolean bringtotop = true
boolean titlebar = true
string title = "Programación Salida"
string dataobject = "dw_mues_embalajes_etiq_salidas_clon"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event key;IF ib_MultiCalibre OR ib_MultiEmbalaje THEN
	KeyControl(key)
ELSE
	sle_embaladora.SetFocus()
END IF
end event

type sle_embaladora from singlelineedit within w_mant_mues_spro_cajasprod_multilado
integer x = 23
integer y = 1028
integer width = 3520
integer height = 392
integer taborder = 70
boolean bringtotop = true
integer textsize = -65
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_embaladora

IF despiececodcorto(This.Text) THEN
	ii_controllinea	=	0
	IF NOT cargaparametros() THEN 
		This.Text	=	''
		This.SetFocus()
		Return
	END IF
	
	IF NOT CargaPrograma() THEN 
		This.Text	=	''
		This.SetFocus()
		HALT
	END IF
	
	sle_lectura1.text	=	"01" + String(ii_proceso)
	sle_lectura2.text	=	"02" + String(il_lotemenor)
	sle_lectura3.Text	=	"03" + is_embala
	sle_lectura4.Text	=	"04" + is_calib
	
	pb_ok.TriggerEvent(Clicked!)
ELSE
	This.Text	=	''
	This.SetFocus()
END IF

This.SetFocus()
end event

type dw_3 from datawindow within w_mant_mues_spro_cajasprod_multilado
event key pbm_dwnkey
boolean visible = false
integer x = 23
integer y = 8
integer width = 3520
integer height = 1016
integer taborder = 10
string title = "none"
string dataobject = "dw_mant_mues_spro_cajasprod_clon"
boolean border = false
boolean livescroll = true
end type

event key;IF ib_MultiCalibre OR ib_MultiEmbalaje THEN
	KeyControl(key)
ELSE
	sle_embaladora.SetFocus()
END IF
end event

type gb_7 from groupbox within w_mant_mues_spro_cajasprod_multilado
boolean visible = false
integer x = 4311
integer y = 2556
integer width = 251
integer height = 248
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type pb_5 from picturebutton within w_mant_mues_spro_cajasprod_multilado
integer x = 3337
integer y = 2044
integer width = 155
integer height = 132
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
string disabledname = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type gb_6 from groupbox within w_mant_mues_spro_cajasprod_multilado
integer x = 3291
integer y = 1968
integer width = 251
integer height = 248
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

