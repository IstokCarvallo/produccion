$PBExportHeader$uo_tipomovtofruta.sru
$PBExportComments$Objeto de Validación deTipo Movimiento Fruta
forward
global type uo_tipomovtofruta from nonvisualobject
end type
end forward

global type uo_tipomovtofruta from nonvisualobject
end type
global uo_tipomovtofruta uo_tipomovtofruta

type variables
Integer	Codigo, TipoCorrelativo, Sentido, Fuente, &
			FrutaGranel, FrutaEmbalada, FrutaComercial, &
			Envases, SolicitaMotivo, SolicitaOrigDest, SolicitaTrasnport, &
			SolicitaDiasGracia, TipoDocto
String	Nombre
end variables

forward prototypes
public function long ultimocorrelativo (integer ai_modulo, integer ai_tipomovto, integer ai_planta)
public function boolean actualiza (integer ai_planta, integer ai_tipomovto, long al_numero)
public function boolean actualiza_correlativo (integer ai_modulo, integer ai_planta, integer ai_tipomovto, long al_numero)
public subroutine bloqueacorrel ()
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
public function integer ultimocorrel (integer ai_planta, integer ai_tipomovto)
end prototypes

public function long ultimocorrelativo (integer ai_modulo, integer ai_tipomovto, integer ai_planta);Long ll_NumeroMovto

CHOOSE CASE ai_Modulo
	CASE 1	//Fruta Granel
		
		SELECT	IsNull(Max(tpmv_ulcofg), 0) + 1
			INTO	:ll_NumeroMovto
			FROM	dbo.spro_tipomovtofruta tmf,
					dbo.spro_correltipomovto cor
			WHERE	tmf.tpmv_codigo =	:ai_tipomovto
			AND   tmf.tpmv_tipcor = cor.tpmv_tipcor
			AND   cor.plde_codigo = :ai_planta;

	CASE 2	//Fruta Comercial
		
		SELECT	IsNull(Max(tpmv_ulcofc), 0) + 1
			INTO	:ll_NumeroMovto
			FROM	dbo.spro_tipomovtofruta tmf,
					dbo.spro_correltipomovto cor
			WHERE	tmf.tpmv_codigo =	:ai_tipomovto
			AND   tmf.tpmv_tipcor = cor.tpmv_tipcor
			AND   cor.plde_codigo = :ai_planta;

	CASE 3	//Fruta Embalada
		
		SELECT	IsNull(Max(tpmv_ulcofe), 0) + 1
			INTO	:ll_NumeroMovto
			FROM	dbo.spro_tipomovtofruta tmf,
					dbo.spro_correltipomovto cor
			WHERE	tmf.tpmv_codigo =	:ai_tipomovto
			AND   tmf.tpmv_tipcor = cor.tpmv_tipcor
			AND   cor.plde_codigo = :ai_planta;

	CASE 4	//Envases
		SELECT	IsNull(Max(tpmv_ulcoen), 0) + 1
			INTO	:ll_NumeroMovto
			FROM	dbo.spro_tipomovtofruta tmf,
					dbo.spro_correltipomovto cor
			WHERE	tmf.tpmv_codigo =	:ai_tipomovto
			AND   tmf.tpmv_tipcor = cor.tpmv_tipcor
			AND   cor.plde_codigo = :ai_planta;

				/* COMENTADO PARA EVITAR PROBLEMAS CON EL CORRELATIVO DE MOVIMIENTOS
			DE ENVASE, TODO GRACIAS A SPISON.
			SELECT IsNull(Max(meen_numero), 0) + 1
			INTO	:ll_NumeroMovto
			FROM	dbo.spro_movtoenvaenca
			WHERE 	tpmv_codigo 	=	:ai_tipomovto
			    AND	plde_codigo 	=	:ai_planta;*/

END CHOOSE	

RETURN 	ll_NumeroMovto;
end function

public function boolean actualiza (integer ai_planta, integer ai_tipomovto, long al_numero);UPDATE dbo.spro_correltipomovto
	SET	tpmv_ultcor = :al_numero
	FROM dbo.spro_tipomovtofruta smf,dbo.spro_correltipomovto scm
	WHERE	smf.tpmv_codigo = :ai_tipomovto
	AND   smf.tpmv_tipcor = scm.tpmv_tipcor
	AND   scm.plde_codigo = :ai_planta;

	Commit;
	
IF Sqlca.sqlcode <> 0 THEN
	Rollback;
	RETURN False
ELSE
	RETURN True
END IF	
end function

public function boolean actualiza_correlativo (integer ai_modulo, integer ai_planta, integer ai_tipomovto, long al_numero);CHOOSE CASE	ai_modulo
	CASE 1	// Fruta Granel
		UPDATE dbo.spro_correltipomovto
			SET	tpmv_ulcofg = :al_numero
			FROM dbo.spro_tipomovtofruta smf,dbo.spro_correltipomovto scm
			WHERE	smf.tpmv_codigo = :ai_tipomovto
			AND   smf.tpmv_tipcor = scm.tpmv_tipcor
			AND   scm.plde_codigo = :ai_planta;
			
	CASE 2	// Fruta Comercial
		UPDATE dbo.spro_correltipomovto
			SET	tpmv_ulcofc = :al_numero
			FROM dbo.spro_tipomovtofruta smf,dbo.spro_correltipomovto scm
			WHERE	smf.tpmv_codigo = :ai_tipomovto
			AND   smf.tpmv_tipcor = scm.tpmv_tipcor
			AND   scm.plde_codigo = :ai_planta;

	CASE 3	// Fruta Embalada
		UPDATE dbo.spro_correltipomovto
			SET	tpmv_ulcofe = :al_numero
			FROM dbo.spro_tipomovtofruta smf,dbo.spro_correltipomovto scm
			WHERE	smf.tpmv_codigo = :ai_tipomovto
			AND   smf.tpmv_tipcor = scm.tpmv_tipcor
			AND   scm.plde_codigo = :ai_planta;

	CASE 4	// Envases
		UPDATE dbo.spro_correltipomovto
			SET	tpmv_ulcoen = :al_numero
			FROM dbo.spro_tipomovtofruta smf,dbo.spro_correltipomovto scm
			WHERE	smf.tpmv_codigo = :ai_tipomovto
			AND   smf.tpmv_tipcor = scm.tpmv_tipcor
			AND   scm.plde_codigo = :ai_planta;
END CHOOSE

Commit;
	
IF Sqlca.sqlcode <> 0 THEN
	Rollback;
	RETURN False
ELSE
	RETURN True
END IF	
end function

public subroutine bloqueacorrel ();UPDATE dbo.spro_correltipomovto
	SET	tpmv_ultcor = 0
	WHERE	1 = 2;
end subroutine

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	tpmv_codigo, tpmv_nombre, tpmv_tipcor, tpmv_sentid, tpmv_fuente, 
			tpmv_frugra, tpmv_fruemb, tpmv_frucom, tpmv_envase, tpmv_solmot, 
			tpmv_solori, tpmv_soltra, tpmv_soldgr, tdop_codigo
	INTO	:Codigo, :Nombre, :TipoCorrelativo, :Sentido, :Fuente, 
			:FrutaGranel, :FrutaEmbalada, :FrutaComercial, :Envases, :SolicitaMotivo,
			:SolicitaOrigDest, :SolicitaTrasnport, :SolicitaDiasGracia, :TipoDocto
	FROM	dbo.spro_tipomovtofruta
	WHERE	tpmv_codigo	=	:ai_Codigo
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Tipo Movimiento Fruta")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Tipo Movimiento " + &
				String(ai_Codigo, '00') + ", no ha sido registrado.~r~r" + &
				"Ingrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function integer ultimocorrel (integer ai_planta, integer ai_tipomovto);Long ll_NumeroMovto

SELECT	IsNull(Max(tpmv_ultcor), 0) + 1
	INTO	:ll_NumeroMovto
	FROM	dbo.spro_tipomovtofruta tmf,
			dbo.spro_correltipomovto cor
	WHERE	tmf.tpmv_codigo =	:ai_tipomovto
	AND   tmf.tpmv_tipcor = cor.tpmv_tipcor
	AND   cor.plde_codigo = :ai_planta;
	
RETURN 	ll_NumeroMovto;
	

end function

on uo_tipomovtofruta.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_tipomovtofruta.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

