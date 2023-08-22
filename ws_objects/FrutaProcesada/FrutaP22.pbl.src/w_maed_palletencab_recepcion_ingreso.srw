$PBExportHeader$w_maed_palletencab_recepcion_ingreso.srw
forward
global type w_maed_palletencab_recepcion_ingreso from w_maed_palletencab
end type
type dw_4 from datawindow within w_maed_palletencab_recepcion_ingreso
end type
type dw_5 from datawindow within w_maed_palletencab_recepcion_ingreso
end type
type dw_6 from datawindow within w_maed_palletencab_recepcion_ingreso
end type
type dw_3 from datawindow within w_maed_palletencab_recepcion_ingreso
end type
end forward

global type w_maed_palletencab_recepcion_ingreso from w_maed_palletencab
integer x = 14
integer y = 336
integer width = 3355
integer height = 1940
windowtype windowtype = response!
event ue_nuevo_pallet ( )
event ue_antesguardar2 ( )
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
dw_3 dw_3
end type
global w_maed_palletencab_recepcion_ingreso w_maed_palletencab_recepcion_ingreso

type variables
Str_mant	istr_mant2, istr_mant6

DataWindowChild	idwc_categorias, idwc_status, idwc_condicion, &
						idwc_tratamiento, idwc_tipofrio, idwc_destino, &
						dw_ptaori,dw_puerto
										  
Boolean		lb_MensPallet, lb_MensPucho,ib_existe_folio, ib_primera_entrada
Integer		ii_CantPallets, ii_CantPuchos
end variables

forward prototypes
public function boolean duplicado (string campo)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicadoinspecdet (long nroinspeccion)
public function boolean duplicadoinspeccion (long nroinspeccion)
public function boolean noexistetarjare (integer cliente, integer planta, integer folio, integer pallet)
public function boolean noexistetarja (integer cliente, integer planta, integer folio, integer pallet)
public function boolean noesnuevo (integer planta, integer cliente, long numero)
public function integer palletsinencab (integer planta, integer cliente, long numero)
public function boolean existepallet (string ls_columna)
public subroutine buscaembarque ()
public subroutine buscaproductor ()
public function boolean existefolio (string as_columna, string as_valor)
public function boolean noexisteembarque (string as_columna, string as_valor)
public function boolean noexisteproductor (string as_columna, string as_valor)
public function boolean noexistecliente (integer ai_codigo)
public function boolean noexisteplanta (string columna)
public subroutine habilitaingreso2 ()
public subroutine habilitaencab (boolean habilita)
public function long buscanuevofolio (integer cliente, integer planta)
end prototypes

event ue_nuevo_pallet();Boolean	lb_TerminaPallet, lb_terminaPucho
Integer	li_Pallets, li_Puchos
Long		ll_modif1, ll_modif2, ll_modif6

ib_ok	= True

CHOOSE CASE  wf_modifica()
	CASE -1
		ib_ok = False
	CASE 0
		ll_modif1	=	dw_1.GetNextModified(0, Primary!)
		ll_modif2	=	dw_2.GetNextModified(0, Primary!)
	
//		IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
		IF dw_1.RowCount() > 0 THEN
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					This.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN ib_ok = False
				CASE 3
					ib_ok	= False
					RETURN
			END CHOOSE
		END IF
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
dw_4.Reset()
dw_5.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)

dw_2.Object.paen_numero.Protect	=	0
dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
dw_2.SetRedraw(True)


istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[6]	= 	istr_mant2.argumento[1]
istr_mant.argumento[9]	= 	"1"
istr_mant.argumento[10]	=	"0"

IF ii_CantPallets=0 THEN
	dw_2.SetItem(1, "paen_tipopa", 2)
	dw_2.Object.paen_tipopa.Protect	=	1
	lb_TerminaPallet						=	True
END IF

IF dw_3.RowCount() > 0 THEN
	li_Pallets	=	dw_3.GetItemNumber(1, "pallets")
	li_Puchos	=	dw_3.GetItemNumber(1, "puchos")

	IF ii_CantPallets > 0 AND dw_3.GetItemNumber(1, "pallets") = ii_CantPallets THEN
		IF Not lb_MensPallet THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Pallets~r" + &
							"indicados en la Recepción")
			
			lb_MensPallet	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 2)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPallet						=	True
	END IF
	
	IF ii_CantPuchos > 0 AND dw_3.GetItemNumber(1, "puchos") = ii_CantPuchos THEN
		IF Not lb_MensPucho THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Puchos~r" + &
							"indicados en la Recepción")
			
			lb_MensPucho	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 1)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPucho						=	True
	END IF
	
	IF lb_TerminaPallet AND lb_TerminaPucho THEN
		pb_salir.TriggerEvent(Clicked!)
	ELSE
		istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[6]	=	istr_mant2.argumento[1]
		istr_mant.argumento[9]	=	"1"
		istr_mant.argumento[10]	=	"0"
	END IF
END IF

dw_2.Enabled		=	False
dw_3.Enabled		=	False
end event

event ue_antesguardar2();Long 		ll_nuevofolio
Integer	li_fillas

IF dw_6.GetNextModified(0, Primary!) > 0 THEN
	dw_6.SetItem(1, "rfpe_fecact", Today())
	dw_6.SetItem(1, "rfpe_horact", Now())
END IF

//IF Not ib_primera_entrada AND Not ib_existe_folio  THEN
	
	/*
	Se actualiza tabla recfruprocee a objeto de bloquearla hasta que termine la grabación
	del ingreso
	*/
	UPDATE dba.RECFRUPROCEE SET
			 rfpe_guides = 999
			 WHERE rfpe_tarjas = 999
			 AND   rfpe_nrores = 999
			 AND   rfpe_tardef = 999;

	ll_nuevofolio=Buscanuevofolio(Integer(istr_mant6.argumento[3]),Integer(istr_mant6.argumento[1]))
	dw_6.Object.rfpe_numero[1]	= ll_nuevofolio
   dw_6.SetItem(1, "rfpe_numero",ll_nuevofolio)

	istr_mant6.argumento[2]	= String(ll_nuevofolio)
	
//	FOR li_fillas = 1 TO dw_6.RowCount()
//		 dw_3.Object.rfpe_numero[li_fillas]	= ll_nuevofolio
//	NEXT		


//END IF
end event

public function boolean duplicado (string campo);Long		ll_fila
String	ls_codigo,ls_cliente,ls_numero

ls_codigo	= String(dw_2.Object.plde_codigo[1])
ls_numero	= istr_mant2.Argumento[2]
ls_cliente	= String(dw_2.Object.clie_codigo[1])

ll_fila	= dw_3.Find("plde_codigo = " + ls_codigo + " AND rfpe_numero = " + ls_numero + &
					" AND clie_codigo = " + ls_cliente + " AND paen_numero =	" + campo, &
					1, dw_3.RowCount())

IF ll_fila > 0 THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

protected function boolean wf_actualiza_db (boolean borrando);if not dw_2.uf_check_required(0) then return false
if not dw_1.uf_validate(0) then return false
if borrando then
	if dw_1.Update() = 1 and dw_2.Update() = 1 then
		commit;
		if sqlca.sqlcode <> 0 then
				f_errorbasedatos(sqlca,"Lista la tabla")
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
		return false
	end if
else
	if dw_2.update() = 1 and dw_1.Update() = 1 and dw_6.update() = 1 and dw_3.update() = 1 and dw_5.Update() = 1 and dw_4.Update() = 1 then 
		commit;
		if sqlca.sqlcode <> 0 then
			f_errorbasedatos(sqlca,"Lista la tabla")
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
		return false
	end if
end if
return true
end function

public function boolean duplicadoinspecdet (long nroinspeccion);Long		ll_fila, ll_numero
Integer  li_planta,li_cliente,li_tipoin, li_secuen
Date		ld_fechai

SetNull(li_secuen)

li_tipoin	=	1
li_planta	= dw_2.Object.plde_codigo[1]
li_cliente	= dw_2.Object.clie_codigo[1]
ll_numero   = dw_2.Object.paen_numero[1]

ll_fila	= dw_4.Find("inpe_tipoin = " + String(li_tipoin) + " AND inpe_numero = " + String(nroinspeccion) + &
               " AND paen_numero = " + String(ll_numero) + &
               " AND plde_codigo = " + String(li_planta) + " AND clie_codigo = " + String(li_cliente), &
					1, dw_4.RowCount())

IF ll_fila > 0 THEN
	RETURN True
ELSE

  SELECT inpe_secuen INTO :li_secuen
	FROM dba.INSPECPALDET
	WHERE inpe_tipoin = :li_tipoin
	AND   inpe_numero = :nroinspeccion
	AND   clie_codigo = :li_cliente
	AND   plde_codigo = :li_planta
	AND   paen_numero = :ll_numero;
	
	IF sqlca.SQLCode = -1 THEN
		RETURN True
	ELSEIF Isnull(li_secuen) THEN
		    RETURN False
		ELSE
			 RETURN True
		END IF
   END IF

end function

public function boolean duplicadoinspeccion (long nroinspeccion);Long		ll_fila
Integer  li_planta,li_cliente,li_tipoin,li_secuen
Date		ld_fechai

SetNull(li_secuen)

li_tipoin	=	1
li_planta	= dw_2.Object.plde_codigo[1]
li_cliente	= dw_2.Object.clie_codigo[1]

ll_fila	= dw_5.Find("inpe_tipoin = " + String(li_tipoin) + " AND inpe_numero = " + String(nroinspeccion) + &
               " AND plde_codigo = " + String(li_planta) + " AND clie_codigo = " + String(li_cliente), &
					1, dw_5.RowCount())

IF ll_fila > 0 THEN
	RETURN True
ELSE

	SELECT inpe_secuen INTO :li_secuen
	FROM dba.INSPECPALENC
	WHERE inpe_tipoin = :li_tipoin
	AND   inpe_numero = :nroinspeccion
	AND   clie_codigo = :li_cliente
	AND   plde_codigo = :li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		RETURN True
	ELSEIF Isnull(li_secuen) THEN
		    RETURN False
		ELSE
			 RETURN True
		END IF
   END IF

end function

public function boolean noexistetarjare (integer cliente, integer planta, integer folio, integer pallet);Long	registros
  

  
  SELECT Count(paen_numero)  
    INTO :registros
    FROM dba.Repalletdeta
   WHERE plde_codigo = :planta AND  
         repe_numero = :folio AND  
         clie_codigo = :cliente AND  
         paen_numero = :pallet;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletdeta")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean noexistetarja (integer cliente, integer planta, integer folio, integer pallet);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dba.recfruproced
  	WHERE plde_codigo	=	:planta
  	AND	rfpe_numero	=	:folio
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:pallet ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean noesnuevo (integer planta, integer cliente, long numero);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dba.palletencab
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	RETURN False
ELSEIF registros > 0 THEN
	RETURN True
END IF

RETURN False
end function

public function integer palletsinencab (integer planta, integer cliente, long numero);Long	registros=0,registros1=0, registros2=0
  
SELECT	Count(*)  
	INTO	:registros1
  	FROM	dba.recfruproced
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	registros1=1
END IF

SELECT	Count(*)  
	INTO	:registros2
  	FROM	dba.repalletdeta
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletdeta")
	registros2=1
END IF

registros = registros1 + registros2

RETURN registros
end function

public function boolean existepallet (string ls_columna);Integer		li_cliente, li_tiporec, li_ptadest, li_ptaorig, li_cantid, li_sinencabezado
Long			ll_nropal, ll_fila, ll_filad
String		ls_embarque, ls_embalaje
Boolean		lb_retorno = False
DataStore	ds_encabezado, ds_detalle

ds_encabezado	 =	CREATE DataStore
ds_detalle		 =	CREATE DataStore

li_cliente					= 	dw_2.Object.clie_codigo[1]
li_ptadest					=	dw_2.Object.plde_codigo[1]
li_tiporec					=	Integer(istr_mant.argumento[20])
li_ptaorig					=	Integer(istr_mant.argumento[21])
ls_embarque					=	istr_mant.argumento[24]
ll_nropal 					= 	Long(ls_columna)
istr_mant.argumento[2]	= 	String(ll_nropal)
ls_Embalaje					=	EmbalajeCliente(li_Cliente)

li_sinencabezado			=	PalletSinEncab(li_ptaorig,li_Cliente,ll_nropal)

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.palletencab
	WHERE	clie_codigo		= :li_cliente
	AND	paen_numero    = :ll_nropal ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF li_cantid > 0 THEN
	CHOOSE CASE li_tiporec
		CASE 0,1,4,5
			
//			IF li_sinencabezado > 0 THEN
				MessageBox("Atención","Nro. de Pallet ya existe para este Cliente.  Ingrese otro.", &
					Exclamation!, Ok!)
				lb_retorno = True
			
//			END IF			
			
		CASE 2
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dba.palletencab
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptaorig ;
							
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid > 0 THEN
				ds_encabezado.DataObject	=	"dw_mant_palletencab"
				ds_detalle.DataObject		=	"dw_mues_palletfruta"
				
				ds_encabezado.GetChild("clie_codigo", dw_cliente)
				ds_encabezado.GetChild("plde_codigo", dw_planta)
				ds_encabezado.GetChild("espe_codigo", dw_especie)
				ds_encabezado.GetChild("etiq_codigo", dw_etiqueta)
				ds_encabezado.GetChild("tpem_codigo", dw_emba)
				
				dw_cliente.SetTransObject(sqlca)
				dw_planta.SetTransObject(sqlca)
				dw_especie.SetTransObject(sqlca)
				dw_etiqueta.SetTransObject(sqlca)
				dw_emba.SetTransObject(sqlca)
				
				dw_cliente.Retrieve(li_cliente)
				dw_planta.Retrieve(1)
				dw_especie.Retrieve()
				dw_etiqueta.Retrieve()
				dw_emba.Retrieve(li_cliente, ls_Embalaje)
								
				ds_encabezado.SetTransObject(sqlca)
				ds_detalle.SetTransObject(sqlca)

				ds_encabezado.Retrieve(li_cliente, ll_nropal, li_ptaorig)
				ds_detalle.Retrieve(li_cliente, ll_nropal, li_ptaorig)
				//
				dw_2.SetItem(1, "paen_tipopa", ds_encabezado.Object.paen_tipopa[1])
				dw_2.SetItem(1, "tpem_codigo", ds_encabezado.Object.tpem_codigo[1])
				dw_2.SetItem(1, "espe_codigo", ds_encabezado.Object.espe_codigo[1])
				dw_2.SetItem(1, "vari_codigo", ds_encabezado.Object.vari_codigo[1])
				dw_2.SetItem(1, "vari_nombre", ds_encabezado.Object.vari_nombre[1])
				dw_2.SetItem(1, "tiem_codigo", ds_encabezado.Object.tiem_codigo[1])
				dw_2.SetItem(1, "emba_codigo", ds_encabezado.Object.emba_codigo[1])
				dw_2.SetItem(1, "emba_nombre", ds_encabezado.Object.emba_nombre[1])
				dw_2.SetItem(1, "cate_codigo", ds_encabezado.Object.cate_codigo[1])
				dw_2.SetItem(1, "etiq_codigo", ds_encabezado.Object.etiq_codigo[1])
				dw_2.SetItem(1, "stat_codigo", ds_encabezado.Object.stat_codigo[1])
				dw_2.SetItem(1, "trat_codigo", ds_encabezado.Object.trat_codigo[1])
				dw_2.SetItem(1, "frio_codigo", ds_encabezado.Object.frio_codigo[1])
				dw_2.SetItem(1, "cond_codigo", ds_encabezado.Object.cond_codigo[1])
				dw_2.SetItem(1, "dest_codigo", ds_encabezado.Object.dest_codigo[1])
				dw_2.SetItem(1, "paen_fecemb", ds_encabezado.Object.paen_fecemb[1])
				dw_2.SetItem(1, "paen_cosecha", ds_encabezado.Object.paen_cosecha[1])
				dw_2.SetItem(1, "paen_altura", ds_encabezado.Object.paen_altura[1])
				dw_2.SetItem(1, "paen_ccajas", ds_encabezado.Object.paen_ccajas[1])
				dw_2.SetItem(1, "tmvp_codigo", ds_encabezado.Object.tmvp_codigo[1])
				dw_2.SetItem(1, "paen_fecini", ds_encabezado.Object.paen_fecini[1])
				dw_2.SetItem(1, "paen_horain", ds_encabezado.Object.paen_horain[1])
				dw_2.SetItem(1, "cama_codigo", ds_encabezado.Object.cama_codigo[1])
				dw_2.SetItem(1, "paen_calle", ds_encabezado.Object.paen_calle[1])

				dw_2.SetItem(1, "paen_base", ds_encabezado.Object.paen_base[1])
				dw_2.SetItem(1, "paen_posici", ds_encabezado.Object.paen_posici[1])
				dw_2.SetItem(1, "paen_estado", 1)
				dw_2.SetItem(1, "paen_inspec", ds_encabezado.Object.paen_inspec[1])
				dw_2.SetItem(1, "paen_concal", ds_encabezado.Object.paen_concal[1])
				dw_2.SetItem(1, "paen_pexpor", ds_encabezado.Object.paen_pexpor[1])
				dw_2.SetItem(1, "paen_pmixto", ds_encabezado.Object.paen_pmixto[1])
				dw_2.SetItem(1, "copa_codigo", ds_encabezado.Object.copa_codigo[1])
				//
				FOR ll_fila	=	1 TO ds_detalle.RowCount()
					ll_filad	=	dw_1.InsertRow(0)
					dw_1.SetItem(ll_filad, "clie_codigo", li_cliente)
					dw_1.SetItem(ll_filad, "paen_numero", ll_nropal)
					dw_1.SetItem(ll_filad, "espe_codigo", ds_detalle.Object.espe_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "vari_codigo", ds_detalle.Object.vari_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "emba_codigo", ds_detalle.Object.emba_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "prod_codigo", ds_detalle.Object.prod_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "productores_prod_nombre", ds_detalle.Object.productores_prod_nombre[ll_fila])
					dw_1.SetItem(ll_filad, "cond_codigo", ds_detalle.Object.cond_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "etiq_codigo", ds_detalle.Object.etiq_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "plde_codigo", li_ptadest)
					dw_1.SetItem(ll_filad, "pafr_calibr", ds_detalle.Object.pafr_calibr[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_secuen", ds_detalle.Object.pafr_secuen[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_ccajas", ds_detalle.Object.pafr_ccajas[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_nrlote", ds_detalle.Object.pafr_nrlote[ll_fila])
				NEXT
				istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
				istr_mant.argumento[4]	=	String(dw_2.Object.vari_codigo[1])
				istr_mant.argumento[5]	= 	dw_2.Object.vari_nombre[1]
				istr_mant.argumento[6]	= 	String(dw_2.Object.plde_codigo[1])
				istr_mant.argumento[7]	= 	dw_2.Object.emba_codigo[1]
				istr_mant.argumento[8]	= 	dw_2.Object.emba_nombre[1]
				istr_mant.argumento[9]	= 	String(dw_2.Object.etiq_codigo[1])
				istr_mant.argumento[10] = 	String(dw_2.Object.cond_codigo[1])
				istr_mant.argumento[12] = 	""
				cuentacajas()
				ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
				ExistePlanta(dw_2.Object.plde_codigo[1])
				Existecondicion(dw_2.Object.cond_codigo[1])
				IF noesnuevo(li_ptadest,li_cliente, ll_nropal) THEN
					pb_eliminar.Enabled	= False
					pb_grabar.Enabled		= False
					UPDATE dba.palletencab	SET
					   paen_estado = 1
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest ;
					commit;
				ELSE
					pb_eliminar.Enabled	= True
					pb_grabar.Enabled		= True
				END IF

				END IF
		CASE 3
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dba.palletencab
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptadest ;
							
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid = 0 THEN
				MessageBox("Atención","Nro. de Pallet no Existe en esta Planta.", &
					Exclamation!, Ok!)
				lb_retorno = True
			ELSE
				SELECT	Count(*)
					INTO 	:li_cantid
					FROM	dba.despafrigode as det, dba.despafrigoen as enc
					WHERE	det.plde_codigo	=	:li_ptadest
					AND	det.clie_codigo	=	:li_cliente
					AND	det.paen_numero	=	:ll_nropal
					AND	enc.plde_codigo	=	det.plde_codigo
					AND	enc.defe_numero	=	det.defe_numero
					AND	enc.embq_codigo	=	:ls_embarque ;
				
				IF sqlca.SQLCode = -1 THEN
					F_errorbasedatos(sqlca,"Lectura tablas Despafrigode / Despafrigoen")
					lb_retorno = True
				ELSEIF li_cantid = 0 THEN
					MessageBox("Atención","Nro. de Pallet no existe Despachado para este Embarque.", &
						Exclamation!, Ok!)
					lb_retorno = True
				ELSE
					This.TriggerEvent("ue_recuperadatos")
					istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
					istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
					istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
					istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
					istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
					istr_mant.argumento[8] = dw_2.Object.emba_nombre[1]
					istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
					istr_mant.argumento[10] = String(dw_2.Object.cond_codigo[1])
					istr_mant.argumento[12] = ""
					ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
					ExistePlanta(dw_2.Object.plde_codigo[1])
					Existecondicion(dw_2.Object.cond_codigo[1])
					dw_2.SetItem(1, "paen_estado", 1)
				END IF
			END IF
					
	END CHOOSE
END IF

DESTROY ds_encabezado
DESTROY ds_detalle

RETURN lb_retorno
end function

public subroutine buscaembarque ();Str_busqueda	lstr_busq

dw_6.Modify("buscaembarque.border = 5")

istr_busq.Argum[1]	=	String(dw_6.Object.clie_codigo[1])

OpenWithParm(w_busc_embarques, istr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.Argum[1] <> "" THEN
	dw_6.setItem(1, "embq_codigo", lstr_busq.argum[1])
	dw_6.setItem(1, "embq_nomnav", lstr_busq.argum[2])
ELSE
	dw_6.SetColumn("embq_codigo")
	dw_6.SetFocus()
END IF

dw_6.Modify("buscaembarque.border = 6")


//HabilitaIngreso()
end subroutine

public subroutine buscaproductor ();Str_busqueda	lstr_busq

dw_6.Modify("buscaproductor.border = 5")

lstr_busq.Argum[1]	=	istr_mant6.Argumento[3]

OpenWithParm(w_busc_productores, istr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	dw_6.setItem(1, "prod_codigo", lstr_busq.argum[3])
	dw_6.setItem(1, "prod_nombre", lstr_busq.argum[4])
ELSE
	dw_6.SetColumn("prod_codigo")
	dw_6.SetFocus()
END IF

dw_6.Modify("buscaproductor.border = 6")
end subroutine

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe,li_cliente, li_tipoen
Long		ll_nfolio

li_planta	=	dw_6.Object.plde_codigo[1]
ll_nfolio 	=	dw_6.Object.rfpe_numero[1]
li_cliente	=  dw_6.Object.clie_codigo[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "rfpe_numero"
		ll_nfolio 	=	Long(as_valor)
		
	CASE "clie_codigo"
		li_cliente 	=	Integer(as_valor)	
		
END CHOOSE

SELECT  	rfpe_tipoen
	INTO	:li_tipoen
	FROM	dba.RECFRUPROCEE
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio
	AND   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
	RETURN False
ELSEIF sqlca.SQLCode = 0 THEN
		 IF li_tipoen = 1 THEN
		    dw_ptaori.Setfilter("plde_tipopl=2")
		    dw_ptaori.Filter()
	    ELSE
		    dw_ptaori.Setfilter("plde_tipopl=1")
		    dw_ptaori.Filter()
	    END IF
	    istr_mant6.argumento[1]	= String(li_planta)
	    istr_mant6.argumento[2]	= String(ll_nfolio)
       istr_mant6.argumento[3]	= String(li_cliente) 
	
	    istr_mant6.argumento[4]	= String(dw_2.Object.rfpe_tarjas[1])
	    dw_6.SetItem(1, "clie_codigo",li_cliente)
	    dw_6.SetItem(1, "plde_codigo",li_planta)
	    This.TriggerEvent("ue_recuperadatos")
		 ib_existe_folio	=	True
	    RETURN False
	ELSE
	    IF IsNull(ll_nfolio) THEN
   		 istr_mant6.argumento[1]	= String(li_planta)
		    istr_mant6.argumento[2]	= String(ll_nfolio)
		    istr_mant6.argumento[3]	= String(li_cliente)
			 ib_existe_folio	=	False
		    RETURN False
	    ELSE
		    MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
 			 ib_existe_folio	=	False
		    RETURN True
	    END IF
    END IF

end function

public function boolean noexisteembarque (string as_columna, string as_valor);String	ls_nombre, ls_codigo
Date		ld_fzarpe

ls_codigo	=	dw_6.Object.embq_codigo[1]

CHOOSE CASE as_Columna
	CASE "embq_codigo"
		ls_codigo	=	as_valor

END CHOOSE

IF IsNull(ls_codigo) = False AND ls_codigo <> "" THEN
	SELECT	embq_nomnav, embq_fzarpe INTO :ls_nombre, :ld_fzarpe
		FROM	dba.embarqueprod
		WHERE	embq_codigo	=	:ls_codigo ;
				
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención ", "Código de Embarque no ha sido ingresado." + &
						"~n~nIngrese o Seleccione otro.", &
						Exclamation!, OK!) 
		RETURN True
	ELSE
		istr_mant6.argumento[24]	=	ls_codigo
		dw_6.SetItem(1, "embq_nomnav", ls_nombre)
		istr_mant6.argumento[29]	=	ls_nombre
		RETURN False
	END IF
ELSE
	RETURN False
END IF

end function

public function boolean noexisteproductor (string as_columna, string as_valor);String	ls_nombre
Long     ll_product

ll_product	=	dw_6.Object.prod_codigo[1]

CHOOSE CASE as_Columna
	CASE "prod_codigo"
		ll_product	=	Long(as_valor)
		
END CHOOSE

IF IsNull(ll_product) = False AND ll_product > 0 THEN
	SELECT	prod_nombre INTO :ls_nombre
		FROM	dba.productores
		WHERE	prod_codigo	=	:ll_product ;
				
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención ", "Código de Productor no ha sido ingresado." + &
						"~n~nIngrese o Seleccione otro.", &
						Exclamation!, OK!) 
		RETURN True
	ELSE
		dw_6.SetItem(1, "prod_codigo", ls_nombre)
		RETURN False
	END IF
ELSE
	RETURN False
END IF
end function

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dba.clientesprod
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	dw_6.GetChild("plde_codigo", dw_planta)
	dw_6.GetChild("rfpe_ptaori", dw_ptaori)
	dw_planta.SetTransObject(sqlca)
	dw_ptaori.SetTransObject(sqlca)
	istr_mant6.Argumento[3]	=	String(ai_codigo)
	dw_planta.Retrieve(Integer(istr_mant6.argumento[3]), 1)
	dw_ptaori.Retrieve(Integer(istr_mant6.argumento[3]))
	RETURN False
ELSE
	RETURN True
END IF

end function

public function boolean noexisteplanta (string columna);Integer	li_planta, li_tipo

li_planta	=	Integer(columna)
li_tipo		=	Integer(istr_mant6.argumento[13])

SELECT	plde_codigo
	INTO	:li_planta 
   FROM	dba.plantadesp  
   WHERE plde_codigo	=	:li_planta
	AND	plde_tipopl	=	:li_tipo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Planta/Packing no Existe. Ingrese otro")
	RETURN True
END IF

istr_mant6.argumento[15]	=	String(li_planta)

RETURN False
end function

public subroutine habilitaingreso2 ();Date		ld_fecha
Integer	li_tarjas,li_tardef
Boolean	lb_estado = True
String	ls_patent, ls_chofer

dw_6.AcceptText()

li_tarjas	=	dw_6.Object.rfpe_tarjas[1]
li_tardef	=	dw_6.Object.rfpe_tardef[1]
ls_patent	=	dw_6.Object.rfpe_patent[1]
ls_chofer	=	dw_6.Object.rfpe_chofer[1]

IF IsNull(li_tarjas) THEN li_tarjas = 0
IF IsNull(li_tardef) THEN li_tardef = 0

IF IsNull(dw_6.Object.rfpe_nrores[1]) OR dw_6.Object.rfpe_nrores[1] = 0 OR &
	IsNull(dw_6.Object.tica_codigo[1]) OR dw_6.Object.tica_codigo[1] = 0 OR &
	IsNull(ls_patent) OR ls_patent = "" OR &
	IsNull(dw_6.Object.tran_codigo[1]) OR dw_6.Object.tran_codigo[1] = 0 OR &
	IsNull(ls_chofer) OR ls_chofer = "" OR &
	li_tarjas + li_tardef = 0 THEN
	lb_estado = False
END IF

CHOOSE CASE dw_6.Object.rfpe_tipoen[1]
	CASE 1, 2
		IF IsNull(dw_6.Object.rfpe_ptaori[1]) OR dw_6.Object.rfpe_ptaori[1] = 0 THEN
			lb_estado = False
		END IF
	
	 CASE 3
		IF IsNull(dw_6.Object.puer_codigo[1]) OR dw_6.Object.puer_codigo[1] = 0 OR &
			IsNull(dw_6.Object.embq_codigo[1]) OR dw_6.Object.embq_codigo[1] = "" THEN
			lb_estado = False
		END IF			
END CHOOSE	

IF lb_estado THEN
   dw_2.Enabled		=	True
   dw_3.Enabled		=	True
   //pb_ins_det.Enabled = lb_estado
END IF

istr_mant2 = istr_mant6
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.paen_numero.Protect	=	0
	dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	dw_2.SetColumn("paen_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.paen_numero.Protect	=	1
	dw_2.Object.paen_numero.BackGround.Color = RGB(166,180,210)
	
	IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN
		dw_2.Object.vari_codigo.Protect	=	1
		dw_2.Object.emba_codigo.Protect	=	1
		dw_2.Object.vari_codigo.BackGround.Color = RGB(166,180,210)
		dw_2.Object.emba_codigo.BackGround.Color = RGB(166,180,210)
	END IF
	
END IF
end subroutine

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_cliente
Long		ll_numero

li_cliente	=	cliente
li_planta	=	planta

SELECT max(rfpe_numero) INTO:ll_numero
FROM DBA.RECFRUPROCEE
WHERE	clie_codigo = :li_cliente
AND	plde_codigo = :li_planta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
ELSEIF sqlca.SQLCode = 0 THEN
	ll_numero++

ELSE
	ll_numero=1

END IF


RETURN ll_numero
end function

on w_maed_palletencab_recepcion_ingreso.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.dw_5
this.Control[iCurrent+3]=this.dw_6
this.Control[iCurrent+4]=this.dw_3
end on

on w_maed_palletencab_recepcion_ingreso.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_3)
end on

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_inspec,li_fillas,ll_nuevofolio

IF gb_Repalletizado THEN
	ll_fila	=	dw_3.InsertRow(0)
	dw_3.Object.plde_codigo[ll_fila]	=	dw_2.Object.plde_codigo[1]
	dw_3.Object.clie_codigo[ll_fila]	=	Integer(istr_mant.Argumento[1])
	dw_3.Object.paen_numero[ll_fila]	=	dw_2.Object.paen_numero[1]
	dw_3.Object.repe_numero[ll_fila]	=	Long(istr_mant2.Argumento[2])
	dw_3.Object.plde_codigo[ll_fila]	=	Integer(istr_mant2.Argumento[1])
	dw_3.Object.repd_tipood[ll_fila]	=	2
	dw_3.Object.paen_tipopa[ll_fila]	=	dw_2.Object.paen_tipopa[1]
	dw_3.Object.vari_nombre[ll_fila]	=	dw_2.Object.vari_nombre[1]
	dw_3.Object.emba_codigo[ll_fila]	=	dw_2.Object.emba_codigo[1]
	dw_3.Object.cate_codigo[ll_fila]	=	dw_2.Object.cate_codigo[1]
	dw_3.Object.stat_codigo[ll_fila]	=	dw_2.Object.stat_codigo[1]
   IF dw_2.Object.paen_inspec[1] = 1 THEN
		ll_inspec	=	Long(String(Long(istr_mant3.Argumento[1]),'00000'))
		IF duplicadoinspecdet(ll_inspec)=False THEN
			dw_4.Object.inpe_tipoin[ll_fila] = 1
			dw_4.Object.inpe_numero[ll_fila] = ll_inspec
			dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[1])
			dw_4.Object.plde_codigo[ll_fila] = Integer(istr_mant2.Argumento[1])
			dw_4.Object.inpe_secuen[ll_fila] = 1
			dw_4.Object.paen_numero[ll_fila] = dw_2.Object.paen_numero[1]
			dw_4.Object.inpd_fechai[ll_fila] = Date(istr_mant3.Argumento[2])
		END IF
		IF duplicadoinspeccion(ll_inspec)=False THEN
			dw_5.Object.inpe_tipoin[1] = 1
			dw_5.Object.inpe_numero[1] = ll_inspec
			dw_5.Object.clie_codigo[1] = Integer(istr_mant.Argumento[1])
			dw_5.Object.plde_codigo[1] = Integer(istr_mant2.Argumento[1])
			dw_5.Object.inpe_secuen[1] = 1
			dw_5.Object.dest_codigo[1] = dw_2.Object.dest_codigo[1]
		   dw_5.Object.inpe_fechai[1] = Date(istr_mant3.Argumento[2])			
		END IF			
			
	END IF

ELSE
	IF NoExisteTarja(Integer(istr_mant.Argumento[1]), Integer(istr_mant2.Argumento[1]), &
						Long(istr_mant2.Argumento[2]), dw_2.Object.paen_numero[1]) THEN	
		//	Actualiza Planta y Estado Existencia en caso de ser Recepción Interplanta
		dw_2.SetItem(1, "plde_codigo", Integer(istr_mant2.argumento[1]))
		dw_2.SetItem(1, "paen_estado", 1)

		//	Cambio en Condición
		FOR ll_Fila = 1 TO dw_1.RowCount()
			dw_1.SetItem(ll_Fila, "plde_codigo", dw_2.Object.plde_codigo[1])
			dw_1.SetItem(ll_Fila, "cond_codigo", dw_2.Object.cond_codigo[1])
		NEXT

		ll_fila	=	dw_3.InsertRow(0)
		dw_3.Object.plde_codigo[ll_fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.rfpe_numero[ll_fila]	=	dw_6.Object.rfpe_numero[1]
		dw_3.Object.clie_codigo[ll_fila]	=	dw_2.Object.clie_codigo[1]
		dw_3.Object.paen_numero[ll_fila]	=	dw_2.Object.paen_numero[1]
		dw_3.Object.paen_tipopa[ll_fila]	=	dw_2.Object.paen_tipopa[1]
		dw_3.Object.vari_nombre[ll_fila]	=	dw_2.Object.vari_nombre[1]
		dw_3.Object.emba_codigo[ll_fila]	=	dw_2.Object.emba_codigo[1]
		dw_3.Object.cate_codigo[ll_fila]	=	dw_2.Object.cate_codigo[1]
		dw_3.Object.stat_codigo[ll_fila]	=	dw_2.Object.stat_codigo[1]
		IF dw_2.Object.paen_inspec[1] = 1 THEN
			ll_inspec	=	Long(String(Integer(istr_mant2.Argumento[21]),'00') + String(Long(istr_mant3.Argumento[1]),'0000'))
		   IF duplicadoinspecdet(ll_inspec)=False THEN			
				dw_4.Object.inpe_tipoin[ll_fila] = 1
				dw_4.Object.inpe_numero[ll_fila] = ll_inspec
				dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[1])
				dw_4.Object.plde_codigo[ll_fila] = Integer(istr_mant2.Argumento[1])
 			   dw_4.Object.inpe_secuen[ll_fila] = 1
				dw_4.Object.paen_numero[ll_fila] = dw_2.Object.paen_numero[1]
				dw_4.Object.inpd_fechai[ll_fila] = Date(istr_mant3.Argumento[2])
		   END IF
			IF duplicadoinspeccion(ll_inspec)=False THEN
				dw_5.Object.inpe_tipoin[1] = 1
				dw_5.Object.inpe_numero[1] = ll_inspec
				dw_5.Object.clie_codigo[1] = Integer(istr_mant.Argumento[1])
				dw_5.Object.plde_codigo[1] = Integer(istr_mant2.Argumento[1])
				dw_5.Object.inpe_secuen[1] = 1
				dw_5.Object.dest_codigo[1] = dw_2.Object.dest_codigo[1]
				dw_5.Object.inpe_fechai[1] = Date(istr_mant3.Argumento[2])			
			END IF			

		END IF
	END IF 
	
//	ll_nuevofolio = Long(dw_6.Object.rfpe_numero[1])
//	FOR li_fillas = 1 TO dw_1.RowCount()
//		 dw_3.Object.rfpe_numero[li_fillas]	= ll_nuevofolio
//	NEXT		

END IF
end event

event open;call super::open;x=0
y=0

//	Argumentos Mantenedor
//	istr_mant6.argumento[1]		= 	Código de Planta
//	istr_mant6.argumento[2]		= 	Número de Folio Recepción
//	istr_mant6.argumento[3]		= 	Código de Exportador
//	istr_mant6.argumento[4]		= 	Cantidad de Tarjas Transitorias
//	istr_mant6.argumento[5]		= 	Tipo de packing
// istr_mant6.argumento[6]  	= 	Número de Pallet
// istr_mant6.argumento[7]  	= 	Parámetro de solo consulta 1=consulta, "":otro
//	istr_mant6.argumento[11]	= 	Cantidad de Tarjas Definitivas
// istr_mant6.argumento[20] 	= 	Tipo de Recepción
// istr_mant6.argumento[21]  	= 	Packing Origen

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_6.GetChild("plde_codigo", dw_planta)
dw_6.GetChild("rfpe_ptaori", dw_ptaori)
dw_6.GetChild("puer_codigo", dw_puerto) 

dw_planta.SetTransObject(sqlca)
dw_ptaori.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_ptaori.Retrieve(gi_codplanta)
dw_ptaori.Setfilter("plde_tipopl=2")
dw_ptaori.Filter()
dw_puerto.Retrieve(900)

This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	False
im_menu.Item[1].Item[6].Enabled		=	False
im_menu.Item[7].Visible					=	False

dw_6.SetTransObject(sqlca)

istr_mant6.dw				 = dw_6
istr_mant6.solo_consulta = False

istr_mant6.argumento[1]	=	String(gi_codplanta)
istr_mant6.argumento[3]	=	String(gi_codexport)
istr_mant6.argumento[5]	=	'dw_mues_recfruproced'
istr_mant6.argumento[6]	= 	'1'
istr_mant6.argumento[13] =	'2'
istr_mant6.argumento[20] =	'1'
istr_mant6.argumento[21] =	''
istr_mant6.argumento[24] =	''
istr_mant6.argumento[4]  =	'0'
istr_mant6.argumento[11] =	'0'
istr_mant6.argumento[11] =	'37'

pb_nuevo.PostEvent(Clicked!)

ib_primera_entrada = True

gb_Repalletizado = False

/*
	Argumentos de istr_mant => Envío a Mantención de Detalle
		Argumento	[1]	=	Código de Exportador
						[2]	=	Número de Pallet
						[3]	=	Código de Especie
						[4]	=	Código de Variedad
						[5]	=	Nombre de Variedad
						[6]	=	Código de Planta
						[7]	=	Código de Embalaje
						[8]	=	Nombre del Embalaje
						[9]	=	Código de Etiqueta
						[10]	=	Código de Condición
						[11]	=	
						[12]	=	
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	Lista de Productores de Palltes
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
						[22]	=	Pallet de Exportación ( 1	=	Si	/	0	=	No
						[23]	=	Pallet de Mixto ( 1	=	Si	/	0	=	No
						[25]	=	Inspeccionado ( 1	=	Si	/	0	=	No
						[26]	=	Destino 


	Argumentos de istr_mant2 => Recepción por Ingreso o Consulta
		Argumento	[1]	=	Código de Planta
						[2]	=	Número de Folio Recepción
						[3]	=	Código de Exportador
						[4]	=	
						[5]	=	Tipo de packing
						[6]	=	Número de Pallet
						[7]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[8]	=	
						[9]	=	Código de Especie
						[10]	=	Código de Variedad
						[11]	=	Tarjas Definitivas
						[12]	=	Cantidad de Tarjas Transitorias
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	Lista de Productores de pallets
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
*/
//This.Height	= 2020
im_menu		= m_principal

Integer	li_cliente, li_planta
String	ls_Embalaje

//dw_2.Object.clie_codigo.Protect	=	1
//dw_2.Object.plde_codigo.Protect	=	1
//dw_2.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
//dw_2.Object.plde_codigo.BackGround.Color = RGB(166,180,210)

/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
istr_mant2.Argumento[20]	=	'0'
istr_mant2.Argumento[21]	=	''
istr_mant2.Argumento[25]	=	'0'
istr_mant2.Argumento[26]	=	'0'

istr_mant2=	istr_mant6

IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
	dw_2.Object.paen_pmixto.Protect	=	0
ELSE
	dw_2.Object.paen_pmixto.Protect	=	1
END IF

dw_3.DataObject			=	istr_mant2.argumento[5]
ii_CantPallets				=	Integer(istr_mant2.Argumento[11])
ii_CantPuchos				=	Integer(istr_mant2.Argumento[4])


IF ii_CantPallets=0 THEN

		dw_2.SetItem(1, "paen_tipopa", 2)
		dw_2.Object.paen_tipopa.Protect	=	1
		
END IF


istr_mant.Argumento[1]	=	istr_mant2.Argumento[3]
istr_mant.Argumento[20]	=	istr_mant2.Argumento[20]
istr_mant.Argumento[21]	=	istr_mant2.Argumento[21]
istr_mant.Argumento[22]	=	"1"		//	Pallet de Exportación
istr_mant.Argumento[23]	=	"0"		//	Pallet Mixto
istr_mant.Argumento[24]	=	istr_mant2.Argumento[24]
istr_mant.Argumento[15] =  istr_mant2.Argumento[8] //Lista de Productores de Pallets
istr_mant.argumento[17] =  istr_mant2.argumento[14]//tipo de pantalla; 1 = repalletizado
istr_mant.argumento[24] =  istr_mant2.argumento[15]//cuando es nuevo repalletizaje

li_cliente					=	Integer(istr_mant2.Argumento[3])
li_planta					=	Integer(istr_mant2.Argumento[1])
ls_Embalaje					=	EmbalajeCliente(li_Cliente)

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_2.GetChild("tpem_codigo", dw_emba)
dw_2.GetChild("cate_codigo", idwc_categorias)
dw_2.GetChild("stat_codigo", idwc_status)
dw_2.GetChild("cond_codigo", idwc_condicion)
dw_2.GetChild("trat_codigo", idwc_tratamiento)
dw_2.GetChild("frio_codigo", idwc_tipofrio)
dw_2.GetChild("dest_codigo", idwc_destino)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_emba.SetTransObject(sqlca)
idwc_categorias.SetTransObject(sqlca)
idwc_status.SetTransObject(sqlca)
idwc_condicion.SetTransObject(sqlca)
idwc_tratamiento.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_destino.SetTransObject(sqlca)

dw_cliente.Retrieve(li_cliente)
dw_planta.Retrieve(1)
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
dw_emba.Retrieve(li_cliente, ls_Embalaje)
idwc_categorias.Retrieve()
idwc_status.Retrieve()
idwc_condicion.Retrieve()
idwc_tratamiento.Retrieve()
idwc_tipofrio.Retrieve()
idwc_destino.Retrieve()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)


dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

istr_mant.Argumento[1]	=	String(li_cliente)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

//istr_mant2.dw.ShareData(dw_3)

istr_mant.Argumento[16]=''

pb_nuevo.TriggerEvent(Clicked!)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
//
end event

event ue_nuevo;Boolean	lb_TerminaPallet, lb_terminaPucho
Integer	li_Pallets, li_Puchos
Long		ll_modif1, ll_modif2, ll_modif6

ib_ok	= True

CHOOSE CASE  wf_modifica()
	CASE -1
		ib_ok = False
	CASE 0
		ll_modif1	=	dw_1.GetNextModified(0, Primary!)
		ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		ll_modif6	=	dw_6.GetNextModified(0, Primary!)
	
//		IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
		IF dw_1.RowCount() > 0 THEN
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					This.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN ib_ok = False
				CASE 3
					ib_ok	= False
					RETURN
			END CHOOSE
		END IF
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
//dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
//dw_2.Enabled				= True
dw_6.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)

dw_6.SetRedraw(False)
dw_6.Reset()
dw_6.InsertRow(0)


dw_2.Object.paen_numero.Protect	=	0
dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
dw_2.SetRedraw(True)

dw_6.SetRedraw(True)

dw_6.SetFocus()
dw_6.SetColumn("rfpe_numero")
dw_6.Setitem(1,"clie_codigo", Integer(istr_mant6.argumento[3]))
dw_6.SetItem(1,"plde_codigo", Integer(istr_mant6.argumento[1]))
//dw_6.SetItem(1,"espe_codigo", gi_CodEspecie)



istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[6]	= 	istr_mant2.argumento[1]
istr_mant.argumento[9]	= 	"1"
istr_mant.argumento[10]	=	"0"

IF ii_CantPallets=0 THEN
	dw_2.SetItem(1, "paen_tipopa", 2)
	dw_2.Object.paen_tipopa.Protect	=	1
	lb_TerminaPallet						=	True
END IF

IF dw_3.RowCount() > 0 THEN
	li_Pallets	=	dw_3.GetItemNumber(1, "pallets")
	li_Puchos	=	dw_3.GetItemNumber(1, "puchos")

	IF ii_CantPallets > 0 AND dw_3.GetItemNumber(1, "pallets") = ii_CantPallets THEN
		IF Not lb_MensPallet THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Pallets~r" + &
							"indicados en la Recepción")
			
			lb_MensPallet	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 2)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPallet						=	True
	END IF
	
	IF ii_CantPuchos > 0 AND dw_3.GetItemNumber(1, "puchos") = ii_CantPuchos THEN
		IF Not lb_MensPucho THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Puchos~r" + &
							"indicados en la Recepción")
			
			lb_MensPucho	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 1)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPucho						=	True
	END IF
	
	IF lb_TerminaPallet AND lb_TerminaPucho THEN
		pb_salir.TriggerEvent(Clicked!)
	ELSE
		istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[6]	=	istr_mant2.argumento[1]
		istr_mant.argumento[9]	=	"1"
		istr_mant.argumento[10]	=	"0"
	END IF
END IF

dw_2.Enabled		=	False
dw_1.Enabled		=	False


end event

event ue_seleccion;//
end event

event ue_modifica_detalle();IF istr_mant2.argumento[7] <> '1' THEN
	Cuentacajas()
	
	IF dw_1.RowCount() > 0 THEN
		istr_mant.argumento[1]	=	String(dw_2.getitemNumber(1,"clie_codigo"))
		istr_mant.argumento[2]	=	String(dw_2.getitemNumber(1,"paen_numero"))
		istr_mant.argumento[3]	=	String(dw_2.getitemNumber(1,"espe_codigo"))
//		istr_mant.argumento[4]	=	String(dw_2.getitemNumber(1,"vari_codigo"))
		
//		istr_mant.argumento[4]	=	String(dw_1.Object.vari_codigo[il_fila]) 
//	   istr_mant.argumento[7]	=	dw_1.Object.emba_codigo[il_fila] 
		
		istr_mant.agrega 			= False
		istr_mant.solo_consulta	= False

		
		OpenWithParm(iw_mantencion, istr_mant)
	END IF
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;IF dw_1.RowCount() > 0 THEN
	dw_1.Enabled				=	False
	pb_eli_det.Enabled		=	False
	pb_ins_det.Enabled		=	False
ELSE
	dw_1.Enabled				=	True
	pb_ins_det.Enabled		=	True
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN


PostEvent("ue_nuevo_pallet")

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

IF IsNull(dw_6.Object.rfpe_numero[1]) OR dw_6.Object.rfpe_numero[1]=0 THEN
	TriggerEvent("ue_antesguardar2")
END IF

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF


end event

event resize;//
end event

type dw_1 from w_maed_palletencab`dw_1 within w_maed_palletencab_recepcion_ingreso
integer x = 18
integer y = 1524
integer width = 2926
integer height = 436
integer taborder = 140
boolean titlebar = false
string dataobject = "dw_mues_palletfruta_ingreso"
end type

type dw_2 from w_maed_palletencab`dw_2 within w_maed_palletencab_recepcion_ingreso
integer x = 37
integer y = 672
integer width = 2926
integer height = 840
integer taborder = 80
string dataobject = "dw_mant_palletencab_ingreso"
end type

event dw_2::itemchanged;Long		ll_null
String	ls_columna

SetNull(ll_null)
ls_columna = GetColumnName()

CHOOSE CASE ls_columna

	CASE "paen_inspec"
		IF Integer(data) = 1 THEN
			OpenWithParm(w_proc_inspeccion_informada, istr_mant3)
			istr_mant3 = Message.PowerObjectParm
		END IF

END CHOOSE

call super::itemchanged

IF ii_yaexiste = 1 THEN
	ii_yaexiste = 0
	dw_2.SetItem(1, "paen_numero", ll_null)
	RETURN 1
END IF	
	
end event

type pb_nuevo from w_maed_palletencab`pb_nuevo within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 124
integer taborder = 30
end type

type pb_eliminar from w_maed_palletencab`pb_eliminar within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 352
integer taborder = 40
end type

type pb_grabar from w_maed_palletencab`pb_grabar within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 552
integer taborder = 50
end type

event pb_grabar::clicked;dw_2.SetItem(1, "clie_codigo", Integer(istr_mant6.argumento[3]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant6.argumento[1]))

istr_mant.argumento[1]	=	istr_mant6.argumento[3]
istr_mant.argumento[6]	=	istr_mant6.argumento[1]


dw_6.Enabled	=	False

dw_2.SetColumn('paen_numero')
dw_2.SetFocus()

call super::clicked
end event

type pb_imprimir from w_maed_palletencab`pb_imprimir within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 772
integer taborder = 60
end type

type pb_salir from w_maed_palletencab`pb_salir within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 1028
integer taborder = 70
end type

event pb_salir::clicked;Integer	li_respue

IF istr_mant2.agrega = False AND istr_mant2.borra = False THEN
	CloseWithReturn(Parent, istr_mant2)
ELSE
	IF dw_3.RowCount() < ii_CantPallets + ii_CantPuchos THEN
		li_respue	=	MessageBox("ADVERTENCIA","No se ha completado la cantidad de Pallets~r" + &
							"indicados en la Recepción.~r~rDesea retornar a ventana de recepción.", &
							Question!, YesNo!)
			IF li_respue = 1 THEN
				CloseWithReturn(Parent, istr_mant2)
			ELSE
				pb_nuevo.SetFocus()
			END IF
	ELSE
		CloseWithReturn(Parent, istr_mant2)
	END IF
END IF
	

end event

type pb_ins_det from w_maed_palletencab`pb_ins_det within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 1544
integer taborder = 90
end type

type pb_eli_det from w_maed_palletencab`pb_eli_det within w_maed_palletencab_recepcion_ingreso
integer x = 3081
integer y = 1724
integer taborder = 100
end type

type pb_buscar from w_maed_palletencab`pb_buscar within w_maed_palletencab_recepcion_ingreso
boolean visible = false
integer taborder = 20
end type

type dw_histoencab from w_maed_palletencab`dw_histoencab within w_maed_palletencab_recepcion_ingreso
end type

type dw_histofruta from w_maed_palletencab`dw_histofruta within w_maed_palletencab_recepcion_ingreso
end type

type dw_4 from datawindow within w_maed_palletencab_recepcion_ingreso
boolean visible = false
integer y = 1744
integer width = 3159
integer height = 308
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_inspecpaldet_informada"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_palletencab_recepcion_ingreso
boolean visible = false
integer x = 23
integer y = 1688
integer width = 3104
integer height = 432
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_inspecpalenc_informada"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_palletencab_recepcion_ingreso
integer x = 37
integer y = 36
integer width = 2926
integer height = 656
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_recfruprocee_particular_ingreso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_columna, ls_nula
Date		ld_nula

DataWIndowChild	dw_calibres

SetNull(ls_nula)
SetNull(ld_nula)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "rfpe_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "embq_codigo"
		IF NoExisteEmbarque(ls_columna, data) THEN
			This.SetItem(1, ls_columna, ls_nula)
			RETURN 1
		END IF
		
	CASE "prod_codigo"
		IF NoExisteProductor(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "rfpe_tarjas"
		istr_mant6.argumento[4]	= data
		
		ii_CantPallets				=	Integer(istr_mant6.Argumento[11])
		ii_CantPuchos				=	Integer(istr_mant6.Argumento[4])
		IF ii_CantPallets=0 THEN
				dw_2.SetItem(1, "paen_tipopa", 2)
				dw_2.Object.paen_tipopa.Protect	=	1
			ELSEIF ii_CantPuchos=0 THEN
				dw_2.SetItem(1, "paen_tipopa", 1)
				dw_2.Object.paen_tipopa.Protect	=	1
			ELSE
				dw_2.SetItem(1, "paen_tipopa", 1)
		END IF
		
	CASE "rfpe_tardef"
		istr_mant6.argumento[11]	= data
		ii_CantPallets				=	Integer(istr_mant6.Argumento[11])
		ii_CantPuchos				=	Integer(istr_mant6.Argumento[4])
		IF ii_CantPallets=0 THEN
				dw_2.SetItem(1, "paen_tipopa", 2)
				dw_2.Object.paen_tipopa.Protect	=	1
			ELSEIF ii_CantPuchos=0 THEN
				dw_2.SetItem(1, "paen_tipopa", 1)
				dw_2.Object.paen_tipopa.Protect	=	1
			ELSE
				dw_2.SetItem(1, "paen_tipopa", 1)
		END IF
		

	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF	
		
	CASE "rfpe_tipoen"
		IF (data='1') THEN
			istr_mant6.argumento[13] = '2'
			dw_ptaori.Setfilter("plde_tipopl=2")
			dw_ptaori.Filter()
		ELSEIF (data='2') THEN
			istr_mant6.argumento[13] = '1'
			dw_ptaori.Setfilter("plde_tipopl=1")
			dw_ptaori.Filter()
		END IF
		istr_mant6.argumento[20] = data
		
	CASE "rfpe_ptaori"
		IF NoexistePlanta(data) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE 
			istr_mant6.argumento[21]=data
		END IF
		
		
	CASE "rfpe_fecrec"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		ELSE
			istr_mant6.argumento[30]=data
		END IF
		
	CASE "rfpe_nomres"
			istr_mant6.argumento[31]=data
		
	CASE "rfpe_nrores"
			istr_mant6.argumento[32]=data
			
	CASE "tica_codigo"
			istr_mant6.argumento[33]=data
	CASE "tran_codigo"
		 	istr_mant6.argumento[34]=data
	CASE "rfpe_patent"
			istr_mant6.argumento[35]=data
	CASE "rfpe_chofer"
		 	istr_mant6.argumento[36]=data			 
   
	CASE "puer_codigo"
			istr_mant6.argumento[37]=data
END CHOOSE

HabilitaIngreso2()
end event

event clicked;CHOOSE CASE dwo.name
	CASE "buscaembarque"
		BuscaEmbarque()
		
	CASE "buscaproductor"
		BuscaProductor()
	
END CHOOSE
end event

type dw_3 from datawindow within w_maed_palletencab_recepcion_ingreso
boolean visible = false
integer x = 174
integer y = 1712
integer width = 2935
integer height = 460
integer taborder = 120
boolean bringtotop = true
string dataobject = "dw_mues_recfruproced"
boolean vscrollbar = true
boolean livescroll = true
end type

