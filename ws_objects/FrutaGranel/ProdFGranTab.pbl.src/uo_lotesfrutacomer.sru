$PBExportHeader$uo_lotesfrutacomer.sru
$PBExportComments$Objeto de Validación de Lotes Fruta Granel
forward
global type uo_lotesfrutacomer from nonvisualobject
end type
end forward

global type uo_lotesfrutacomer from nonvisualobject
end type
global uo_lotesfrutacomer uo_lotesfrutacomer

type variables
Integer		Planta, Especie, Numero, Variedad, PeriodoFrio, &
            PreFrio, Tipoenvase, Envase, Turno, Secuencia, Categoria, &
				Servicio, DiasGracia, Tipool
Decimal{3}	TotalNeto, KilosPromed, TotalBulto
Long			NroFolio, SaldoExistencia, Productor
DateTime		ld_Fecham
String		TipoFrio, NomPlanta, NomEspecie, GrupoCalibre, Calibre
end variables

forward prototypes
public function long saldoexistencia (integer ai_especie, integer ai_variedad, long al_productor, string as_tratamiento, integer ai_periodo, transaction at_transaccion)
public function boolean obtienesaldocamara (integer ai_planta, integer ai_camara, ref long al_saldo, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente)
public function boolean existe (integer ai_planta, integer ai_especie, long ai_numero, integer ai_secuen, boolean ab_mensaje, transaction at_transaccion)
public function boolean existeencab (integer ai_planta, integer ai_especie, integer ai_numero, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function long saldoexistencia (integer ai_especie, integer ai_variedad, long al_productor, string as_tratamiento, integer ai_periodo, transaction at_transaccion);
//SELECT	Sum(cam.caex_canbul - IsNull(cam.caex_bulcom,0))
//	INTO	:SaldoExistencia
//	FROM	dba.spro_lotesfrutagrandeta lfd,dba.spro_lotesfrutagranel lfg,
//			dba.spro_camaraexistefg cam
//	WHERE	lfg.lote_espcod =	:ai_Especie
//	AND	lfg.vari_codigo =	:ai_Variedad
//	AND	lfg.prod_codigo =	:al_Productor
//	AND	lfg.frio_tipofr =	:as_Tratamiento
//	AND	lfg.pefr_codigo =	:ai_Periodo
//	AND	lfg.lote_pltcod = lfd.lote_pltcod
//	AND	lfg.lote_espcod = lfd.lote_espcod
//	AND	lfg.lote_codigo = lfd.lote_codigo
//   AND   lfd.lote_pltcod = cam.lote_pltcod
//   AND   lfd.lote_espcod = cam.lote_espcod
//   AND   lfd.lote_codigo = cam.lote_codigo
//   AND   lfd.enva_tipoen = cam.enva_tipoen
//   AND   lfd.enva_codigo = cam.enva_codigo
//   AND   lfd.lotd_kilpro > 0
//	USING at_Transaccion;
//	
//	IF at_Transaccion.SQLCode = -1 THEN
//		F_ErrorBaseDatos(at_Transaccion, "Lectura de Existencia de Bultos")
	
		SaldoExistencia = 0
//	END IF

RETURN SaldoExistencia
end function

public function boolean obtienesaldocamara (integer ai_planta, integer ai_camara, ref long al_saldo, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente);Boolean	lb_Retorno	=	True

SELECT caex_canbul
	INTO	:al_Saldo
	FROM	dba.spro_camaraexistecom
	WHERE	plde_codigo =	:ai_Planta
	AND	cama_codigo =	:ai_Camara
	AND	lofc_pltcod =	:Planta
	AND	lofc_espcod =	:Especie
	AND	lofc_lotefc =	:Numero
	AND   lfcd_secuen =  :Secuencia
	AND   clie_codigo =  :ai_Cliente
	USING at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura Existencia en Cámaras")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 OR Isnull(al_Saldo) OR al_Saldo = 0 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención","No Existe Saldo en la Cámara "+String(ai_Camara,'00')+&
						" para Lote "+String(Planta,'0000')+String(Especie,'00')+String(Numero,'0000'))
	END IF
	
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
					
end function

public function boolean existe (integer ai_planta, integer ai_especie, long ai_numero, integer ai_secuen, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_nombre
	INTO	:NomPlanta
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:ai_Planta
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Plantas")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN

	IF ab_Mensaje	THEN
		MessageBox("Atención", "Código de Planta " + String(ai_Planta, '0000') + &
				", no ha sido definido.~r~rIngrese o seleccione otro Código.")	
	END IF
	lb_Retorno	=	False
END IF

IF lb_Retorno THEN
	SELECT	espe_nombre
		INTO	:NomEspecie
		FROM	dba.especies
		WHERE	espe_codigo	=	:ai_Especie
//		AND   clie_codigo =  :ai_Cliente
		USING	at_Transaccion;

	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Especies")
		
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 THEN
		IF ab_Mensaje	THEN
			MessageBox("Atención", "Código de Especie " + String(ai_Especie, '00') + &
						", no ha sido definido.~r~rIngrese o seleccione otro Código.")	
		END IF
		lb_Retorno	=	False
	END IF
END IF

IF lb_Retorno THEN
	SELECT	lofc_pltcod, lofc_espcod, lofc_lotefc, lfcd_secuen, lfcd_nturno,
	         lfcd_fecham, vari_codigo, prod_codigo, frio_tipofr, pefr_codigo,
				enva_tipoen, enva_codigo, refe_calibr, refe_gcalib, lfcd_bultos,
				lfcd_Kilnet, lfcd_kilpro, lfcd_tipool
		INTO	:Planta, :Especie, :Numero, :Secuencia, :Turno, 
				:ld_Fecham, :Variedad, :Productor, :TipoFrio, :PeriodoFrio,
				:Tipoenvase, :Envase, :Calibre, :Grupocalibre, :TotalBulto,
				:TotalNeto, :KilosPromed, :Tipool
		FROM	dba.spro_lotesfrutacomdeta
		WHERE	lofc_pltcod	=	:ai_Planta
//		AND   clie_codigo =  :ai_Cliente
		AND	lofc_espcod	=	:ai_Especie
		AND	lofc_lotefc	=	:ai_Numero
		AND   lfcd_secuen =  :ai_secuen
		USING at_Transaccion;
	
	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Lotes Fruta Comercial")
		
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 THEN
		IF ab_Mensaje	THEN
			MessageBox("Atención", "Número de Lote " + String(ai_Numero, '0000') + &
						", no ha sido registrado para Planta '" + Trim(NomPlanta) + &
						" y Especie '" + Trim(NomEspecie) + "'.~r~r" + &
						"Ingrese o seleccione otro Lote.")	
		END IF					
		
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existeencab (integer ai_planta, integer ai_especie, integer ai_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_nombre
	INTO	:NomPlanta
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:ai_Planta
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Plantas")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 AND ab_Mensaje	THEN
	MessageBox("Atención", "Código de Planta " + String(ai_Planta, '0000') + &
				", no ha sido definido.~r~rIngrese o seleccione otro Código.")	
	
	lb_Retorno	=	False
END IF

IF lb_Retorno THEN
	SELECT	espe_nombre
		INTO	:NomEspecie
		FROM	dba.especies
		WHERE	espe_codigo	=	:ai_Especie
//		AND   clie_codigo =  :ai_Cliente		
		USING	at_Transaccion;

	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Especies")
		
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 AND ab_Mensaje	THEN
		MessageBox("Atención", "Código de Especie " + String(ai_Especie, '00') + &
					", no ha sido definido.~r~rIngrese o seleccione otro Código.")	
		
		lb_Retorno	=	False
	END IF
END IF

IF lb_Retorno THEN
	SELECT	lofc_pltcod, lofc_espcod, lofc_lotefc, prod_codigo,
	         sepl_codigo, lofc_diagra, lofc_totbul, lofc_totkil
		INTO	:Planta, :Especie, :Numero, :Productor, :Servicio, 
				:DiasGracia, :TotalBulto, :TotalNeto
		FROM	dba.spro_lotesfrutacomenc
		WHERE	lofc_pltcod	=	:ai_Planta
		AND	lofc_espcod	=	:ai_Especie
		AND	lofc_lotefc	=	:ai_Numero
//		AND   clie_codigo =  :ai_Cliente		
		USING at_Transaccion;
	
	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Lotes Fruta Comercial")
		
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 AND ab_Mensaje	THEN
		MessageBox("Atención", "Número de Lote " + String(ai_Numero, '0000') + &
					", no ha sido registrado para Planta '" + Trim(NomPlanta) + &
					" y Especie '" + Trim(NomEspecie) + "'.~r~r" + &
					"Ingrese o seleccione otro Lote.")	
		
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

on uo_lotesfrutacomer.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_lotesfrutacomer.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

