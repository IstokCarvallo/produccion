$PBExportHeader$w_mant_mues_ordencargapallet.srw
$PBExportComments$Mantención de Pallets seleccionados para Embarcar
forward
global type w_mant_mues_ordencargapallet from w_mant_directo
end type
type tab_1 from tab within w_mant_mues_ordencargapallet
end type
type tabpage_1 from userobject within tab_1
end type
type dw_cam01 from datawindow within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_cam01 dw_cam01
end type
type tabpage_2 from userobject within tab_1
end type
type dw_cam02 from datawindow within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_cam02 dw_cam02
end type
type tabpage_3 from userobject within tab_1
end type
type dw_cam03 from datawindow within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_cam03 dw_cam03
end type
type tabpage_4 from userobject within tab_1
end type
type dw_cam04 from datawindow within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_cam04 dw_cam04
end type
type tabpage_5 from userobject within tab_1
end type
type dw_cam05 from datawindow within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_cam05 dw_cam05
end type
type tabpage_6 from userobject within tab_1
end type
type dw_cam06 from datawindow within tabpage_6
end type
type tabpage_6 from userobject within tab_1
dw_cam06 dw_cam06
end type
type tabpage_7 from userobject within tab_1
end type
type dw_cam07 from datawindow within tabpage_7
end type
type tabpage_7 from userobject within tab_1
dw_cam07 dw_cam07
end type
type tabpage_8 from userobject within tab_1
end type
type dw_cam08 from datawindow within tabpage_8
end type
type tabpage_8 from userobject within tab_1
dw_cam08 dw_cam08
end type
type tabpage_9 from userobject within tab_1
end type
type dw_cam09 from datawindow within tabpage_9
end type
type tabpage_9 from userobject within tab_1
dw_cam09 dw_cam09
end type
type tabpage_10 from userobject within tab_1
end type
type dw_cam10 from datawindow within tabpage_10
end type
type tabpage_10 from userobject within tab_1
dw_cam10 dw_cam10
end type
type tabpage_11 from userobject within tab_1
end type
type dw_cam11 from datawindow within tabpage_11
end type
type tabpage_11 from userobject within tab_1
dw_cam11 dw_cam11
end type
type tabpage_12 from userobject within tab_1
end type
type dw_cam12 from datawindow within tabpage_12
end type
type tabpage_12 from userobject within tab_1
dw_cam12 dw_cam12
end type
type tabpage_13 from userobject within tab_1
end type
type dw_cam13 from datawindow within tabpage_13
end type
type tabpage_13 from userobject within tab_1
dw_cam13 dw_cam13
end type
type tabpage_14 from userobject within tab_1
end type
type dw_cam14 from datawindow within tabpage_14
end type
type tabpage_14 from userobject within tab_1
dw_cam14 dw_cam14
end type
type tabpage_15 from userobject within tab_1
end type
type dw_cam15 from datawindow within tabpage_15
end type
type tabpage_15 from userobject within tab_1
dw_cam15 dw_cam15
end type
type tabpage_16 from userobject within tab_1
end type
type dw_cam16 from datawindow within tabpage_16
end type
type tabpage_16 from userobject within tab_1
dw_cam16 dw_cam16
end type
type tabpage_17 from userobject within tab_1
end type
type dw_cam17 from datawindow within tabpage_17
end type
type tabpage_17 from userobject within tab_1
dw_cam17 dw_cam17
end type
type tabpage_18 from userobject within tab_1
end type
type dw_cam18 from datawindow within tabpage_18
end type
type tabpage_18 from userobject within tab_1
dw_cam18 dw_cam18
end type
type tabpage_19 from userobject within tab_1
end type
type dw_cam19 from datawindow within tabpage_19
end type
type tabpage_19 from userobject within tab_1
dw_cam19 dw_cam19
end type
type tabpage_20 from userobject within tab_1
end type
type dw_cam20 from datawindow within tabpage_20
end type
type tabpage_20 from userobject within tab_1
dw_cam20 dw_cam20
end type
type tab_1 from tab within w_mant_mues_ordencargapallet
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
tabpage_6 tabpage_6
tabpage_7 tabpage_7
tabpage_8 tabpage_8
tabpage_9 tabpage_9
tabpage_10 tabpage_10
tabpage_11 tabpage_11
tabpage_12 tabpage_12
tabpage_13 tabpage_13
tabpage_14 tabpage_14
tabpage_15 tabpage_15
tabpage_16 tabpage_16
tabpage_17 tabpage_17
tabpage_18 tabpage_18
tabpage_19 tabpage_19
tabpage_20 tabpage_20
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_ordencargapallet
end type
type st_planta from statictext within w_mant_mues_ordencargapallet
end type
type st_1 from statictext within w_mant_mues_ordencargapallet
end type
type st_2 from statictext within w_mant_mues_ordencargapallet
end type
type uo_seltransporte from uo_seleccion_tipotransporte within w_mant_mues_ordencargapallet
end type
type uo_selorden from uo_seleccion_oredencarga within w_mant_mues_ordencargapallet
end type
end forward

global type w_mant_mues_ordencargapallet from w_mant_directo
integer x = 155
integer y = 156
integer width = 3502
string title = "SELECCION DE PALLET PARA DESPACHO"
event ue_validaborrar ( )
tab_1 tab_1
uo_selplanta uo_selplanta
st_planta st_planta
st_1 st_1
st_2 st_2
uo_seltransporte uo_seltransporte
uo_selorden uo_selorden
end type
global w_mant_mues_ordencargapallet w_mant_mues_ordencargapallet

type variables
uo_plantadesp	iuo_Planta

String	is_Columna, is_Estado
Integer	ii_Calle, ii_Base, ii_Altura, ii_Estado
end variables

forward prototypes
public function boolean duplicado (string as_pallet)
public function boolean noexistepallet (string as_pallet)
public function boolean noexistepallet_ii ()
public subroutine habilitacamara (integer ai_camara, userobject auo_camara, datawindow adw_camara)
public subroutine pueblacamara (integer ai_camara, datawindow adw_camara)
end prototypes

event ue_validaborrar();//Integer li_estado
//
//IF dw_1.GetRow()>0 THEN
//	li_estado	=	dw_1.GetItemNumber(dw_1.GetRow(),"paen_estado")
//	
//	IF li_estado=2 THEN
//		MessageBox("Error", "Pallet fue Despachado, no se puede eliminar " + &
//						"de esta inspección.~r~rIngrese o seleccione otro Pallet.")
//						
//		Message.DoubleParm	=	-1
//	ELSEIF li_estado=3 THEN
//		MessageBox("Error", "Pallet fue Repalletizado, no se puede eliminar " + &
//						"de esta inspección.~r~rIngrese o seleccione otro Pallet.")
//						
//		Message.DoubleParm	=	-1
//	END IF
//END IF
end event

public function boolean duplicado (string as_pallet);Long		ll_fila

ll_fila	= dw_1.Find("paen_numero = " + as_pallet, 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Número de Pallet ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean noexistepallet (string as_pallet);String	ls_variedad, ls_embalaje, ls_especie
Integer	li_cliente, li_tipopa, li_inspec, li_tipoin, li_planta, li_secuen, li_estado, li_fumiga
Long		ll_numero, ll_cajas, ll_nroins

li_cliente	= Integer(istr_mant.argumento[1])
li_planta	= Integer(istr_mant.argumento[2])
li_tipoin	= Integer(istr_mant.argumento[3])
ll_nroins	= Long(istr_mant.argumento[4])
ll_numero 	= Long(as_pallet)

SELECT	pae.paen_tipopa, esp.espe_nombre, var.vari_nombre, emb.emba_nombre,
			pae.paen_ccajas, pae.paen_inspec, pae.paen_estado, pae.cond_codigo
	INTO	:li_tipopa, :ls_especie, :ls_variedad, :ls_embalaje, :ll_cajas,
			:li_inspec, :li_estado, :li_fumiga
	FROM	dba.palletencab as pae, dba.especies as esp,
			dba.variedades as var, dba.embalajes as emb
	WHERE pae.clie_codigo	= :li_cliente
	AND	pae.paen_numero	= :ll_numero
	AND	pae.plde_codigo	= :li_planta
	AND	esp.clie_codigo	= pae.clie_codigo
	AND	esp.espe_codigo	= pae.espe_codigo
	AND	var.clie_codigo	= pae.clie_codigo
	AND	var.espe_codigo	= pae.espe_codigo
	AND	var.vari_codigo	= pae.vari_codigo
	AND	emb.clie_codigo	= pae.clie_codigo
	AND	emb.emba_codigo	= pae.emba_codigo;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Información de Pallet")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado.~rIngrese otro Número.", + &
	Exclamation!, OK!)
	RETURN True
ELSEIF li_tipoin > 0 AND (li_inspec = 1 OR li_inspec = 2) THEN	// Pallet en Existencia Inspeccionado o Reinspeccionado
	SELECT	inpe_secuen
		INTO	:li_secuen
		FROM	dba.inspecpaldet
		WHERE	inpe_tipoin	= :li_tipoin
		AND	inpe_numero = :ll_nroins
		AND	clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		AND	paen_numero	= :ll_numero ;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura Información de Inspección de Pallet")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Pallet ya fue considerado en otra Inspección.~r~r" + &
					"Ingrese o seleccione otro Pallet.")
		RETURN True
	END IF
ELSEIF li_estado = 2 THEN
	MessageBox("Atención","Pallet fue Despachado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_estado = 3 THEN
	MessageBox("Atención","Pallet fue Repalletizado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_tipoin = 2 AND li_inspec = 0 THEN
	MessageBox("Atención","Nro. de Pallet no ha sido previamente Inspeccionado.~r~r" + &
					"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_fumiga <> 0 THEN
	MessageBox("Atención","Pallet está FUMIGADO.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_inspec=3 THEN /*Rechazado SAG*/
	
   	 IF MessageBox("Advertencia", "Inspeccionará un Pallet Rechazado por SAG.~r~r" + &
			            "Desea continuar ?", Question!, YesNo!, 2) = 1 THEN 
          	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
				dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
				dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
				dw_1.SetItem(il_fila, "embalajes_emba_nombre", ls_embalaje)
				dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
				dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
				RETURN False
			ELSE
				RETURN True
			END IF
ELSE	
	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
	dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
	dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "embalajes_emba_nombre", ls_embalaje)
	dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
	dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
	RETURN False
END IF

end function

public function boolean noexistepallet_ii ();String	ls_variedad, ls_embalaje, ls_especie
Integer	li_cliente, li_tipopa, li_inspec, li_tipoin, li_planta, li_secuen, li_estado
Long		ll_numero, ll_cajas, ll_nroins

li_cliente	= Integer(istr_mant.argumento[1])
li_planta	= Integer(istr_mant.argumento[2])
li_tipoin	= Integer(istr_mant.argumento[3])
ll_nroins	= Long(istr_mant.argumento[4])
//ll_numero 	= Long(as_pallet)

SELECT	pae.paen_tipopa, esp.espe_nombre, var.vari_nombre, emb.emba_nombre,
			pae.paen_ccajas, pae.paen_inspec, pae.paen_estado
	INTO	:li_tipopa, :ls_especie, :ls_variedad, :ls_embalaje, :ll_cajas,
			:li_inspec, :li_estado
	FROM	dba.palletencab as pae, dba.especies as esp,
			dba.variedades as var, dba.embalajes as emb
	WHERE pae.clie_codigo	= :li_cliente
	AND	pae.paen_numero	= :ll_numero
	AND	pae.plde_codigo	= :li_planta
	AND	esp.clie_codigo	= pae.clie_codigo
	AND	esp.espe_codigo	= pae.espe_codigo
	AND	var.clie_codigo	= pae.clie_codigo
	AND	var.espe_codigo	= pae.espe_codigo
	AND	var.vari_codigo	= pae.vari_codigo
	AND	emb.clie_codigo	= pae.clie_codigo
	AND	emb.emba_codigo	= pae.emba_codigo;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Información de Pallet")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención",  "Número de Pallet no ha sido creado.~rIngrese otro Número.", + &
	Exclamation!, OK!)
	RETURN True
ELSEIF li_tipoin > 0 AND (li_inspec = 1 OR li_inspec = 2) THEN	// Pallet en Existencia Inspeccionado o Reinspeccionado
	SELECT	inpe_secuen
		INTO	:li_secuen
		FROM	dba.inspecpaldet
		WHERE	inpe_tipoin	= :li_tipoin
		AND	inpe_numero = :ll_nroins
		AND	clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		AND	paen_numero	= :ll_numero ;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura Información de Inspección de Pallet")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		IF li_tipoin = 2 AND li_inspec = 2 THEN
		   MessageBox("Atención","Pallet ya fue considerado en otra Re-Inspección.~r~r" + &
					"Ingrese o seleccione otro Pallet.")
		   RETURN True
		
	   END IF
   END IF
ELSEIF li_estado = 2 THEN
	MessageBox("Atención","Pallet fue Despachado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_estado = 3 THEN
	MessageBox("Atención","Pallet fue Repalletizado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_tipoin = 2 AND li_inspec = 0 THEN
	MessageBox("Atención","Nro. de Pallet no ha sido previamente Inspeccionado.~r~r" + &
					"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_inspec=3 THEN /*Rechazado SAG*/
	
   	 IF MessageBox("Advertencia", "Inspeccionará un Pallet Rechazado por SAG.~r~r" + &
			            "Desea continuar ?", Question!, YesNo!, 2) = 1 THEN 
          	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
				dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
				dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
				dw_1.SetItem(il_fila, "embalajes_emba_nombre", ls_embalaje)
				dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
				dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
				RETURN False
			ELSE
				RETURN True
			END IF
ELSE	
	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
	dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
	dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "embalajes_emba_nombre", ls_embalaje)
	dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
	dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
	RETURN False
END IF

end function

public subroutine habilitacamara (integer ai_camara, userobject auo_camara, datawindow adw_camara);auo_camara.Visible	=	True
auo_camara.Enabled	=	True

adw_camara.DataObject	=	"dw_cons_ordencargapallet"

adw_camara.SetTransObject(sqlca)

adw_camara.Retrieve(uo_SelTransporte.Codigo, &
							uo_SelOrden.Numero, &
							iuo_Planta.Codigo, &
							-1, ai_Camara)

end subroutine

public subroutine pueblacamara (integer ai_camara, datawindow adw_camara);Integer	li_CantCalles
Long		ll_Fila

IF adw_camara.RowCount() > 0 THEN
	li_CantCalles	=	adw_camara.Object.cama_cancal[1]
	
	FOR il_Fila = 1 TO adw_camara.RowCount()
		FOR ii_Calle = 1 TO li_CantCalles
			ii_Base		=	adw_camara.Object.cla1_nrbase[il_Fila]
			ii_Altura	=	adw_camara.Object.cla1_nraltu[il_Fila]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	adw_camara.GetItemNumber(il_Fila, is_Estado)
			ll_Fila		=	dw_1.Find("cama_codigo = " + String(ai_Camara) + &
											 "paen_calle = " + String(ii_Calle) + &
											 "paen_base = " + String(ii_Base) + &
											 "paen_posici = " + String(ii_Altura), &
											 ll_Fila, adw_camara.RowCount())
			dw_1.SetItem(ll_Fila, is_Columna, ii_Estado)
		NEXT
	NEXT
END IF
end subroutine

event open;call super::open;Boolean	lb_Cerrar

This.Width	=	tab_1.Width + 540

IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelTransporte.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelOrden.Numero) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelPlanta.Seleccion(False, False)
	
	uo_SelTransporte.Seleccion(False, False)
	
	uo_SelOrden.Seleccion(False, False)
	
	buscar		=	""
	ordenar		=	""
	iuo_Planta	=	Create uo_plantadesp
	
	uo_SelPlanta.Codigo										=	gi_CodPlanta
	uo_SelPlanta.dw_Seleccion.Object.codigo[1]		=	gi_CodPlanta
	uo_SelPlanta.dw_Seleccion.Object.codigo.Protect	=	1
	
	uo_SelPlanta.TriggerEvent("ue_cambio")
END IF
end event

on w_mant_mues_ordencargapallet.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.uo_selplanta=create uo_selplanta
this.st_planta=create st_planta
this.st_1=create st_1
this.st_2=create st_2
this.uo_seltransporte=create uo_seltransporte
this.uo_selorden=create uo_selorden
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.uo_selplanta
this.Control[iCurrent+3]=this.st_planta
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.uo_seltransporte
this.Control[iCurrent+7]=this.uo_selorden
end on

on w_mant_mues_ordencargapallet.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.uo_selplanta)
destroy(this.st_planta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_seltransporte)
destroy(this.uo_selorden)
end on

event ue_antesguardar;dw_1.Retrieve(uo_SelTransporte.Codigo, &
					uo_SelOrden.Numero, &
					iuo_Planta.Codigo, &
					-1, -1)

IF iuo_Planta.CantCamaras >= 1 THEN &
	PueblaCamara(1, tab_1.tabpage_1.dw_cam01)

IF iuo_Planta.CantCamaras >= 2 THEN &
	PueblaCamara(2, tab_1.tabpage_2.dw_cam02)

IF iuo_Planta.CantCamaras >= 3 THEN &
	PueblaCamara(3, tab_1.tabpage_3.dw_cam03)

IF iuo_Planta.CantCamaras >= 4 THEN &
	PueblaCamara(4, tab_1.tabpage_4.dw_cam04)

IF iuo_Planta.CantCamaras >= 5 THEN &
	PueblaCamara(5, tab_1.tabpage_5.dw_cam05)

IF iuo_Planta.CantCamaras >= 6 THEN &
	PueblaCamara(6, tab_1.tabpage_6.dw_cam06)

IF iuo_Planta.CantCamaras >= 7 THEN &
	PueblaCamara(7, tab_1.tabpage_7.dw_cam07)

IF iuo_Planta.CantCamaras >= 8 THEN &
	PueblaCamara(8, tab_1.tabpage_8.dw_cam08)

IF iuo_Planta.CantCamaras >= 9 THEN &
	PueblaCamara(9, tab_1.tabpage_9.dw_cam09)

IF iuo_Planta.CantCamaras >= 10 THEN &
	PueblaCamara(10, tab_1.tabpage_10.dw_cam10)

IF iuo_Planta.CantCamaras >= 11 THEN &
	PueblaCamara(11, tab_1.tabpage_11.dw_cam11)

IF iuo_Planta.CantCamaras >= 12 THEN &
	PueblaCamara(12, tab_1.tabpage_12.dw_cam12)

IF iuo_Planta.CantCamaras >= 13 THEN &
	PueblaCamara(13, tab_1.tabpage_13.dw_cam13)

IF iuo_Planta.CantCamaras >= 14 THEN &
	PueblaCamara(14, tab_1.tabpage_14.dw_cam14)

IF iuo_Planta.CantCamaras >= 15 THEN &
	PueblaCamara(15, tab_1.tabpage_15.dw_cam15)

IF iuo_Planta.CantCamaras >= 16 THEN &
	PueblaCamara(16, tab_1.tabpage_16.dw_cam16)

IF iuo_Planta.CantCamaras >= 17 THEN &
	PueblaCamara(17, tab_1.tabpage_17.dw_cam17)

IF iuo_Planta.CantCamaras >= 18 THEN &
	PueblaCamara(18, tab_1.tabpage_18.dw_cam18)

IF iuo_Planta.CantCamaras >= 19 THEN &
	PueblaCamara(19, tab_1.tabpage_19.dw_cam19)

IF iuo_Planta.CantCamaras >= 20 THEN &
	PueblaCamara(20, tab_1.tabpage_20.dw_cam20)
end event

event ue_recuperadatos;call super::ue_recuperadatos;//Long	ll_fila, ll_fila1, respuesta
//
//ii_nuevo	=	0
//
//DO
//	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
//	IF ll_fila = -1 THEN
//		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//										Information!, RetryCancel!)
//	ELSE
//		ll_fila1	=	dw_2.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
//									 Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
//		IF ll_fila1 > 0 THEN ii_nuevo	=	1
//		dw_1.SetRow(1)
//		dw_1.SetFocus()
//		pb_eliminar.Enabled	=	True
//		pb_grabar.Enabled		=	True
//		pb_imprimir.Enabled	=	True
//		pb_insertar.SetFocus()
//	END IF
//LOOP WHILE respuesta = 1
//
//IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Long		ll_Filas
//str_info	lstr_info
//
//lstr_info.titulo	= "SOLICITUD INSPECCION FITOSANITARIA S.A.G."
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_compuesto"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//ll_Filas	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
//											Long(istr_mant.argumento[4]), &
//											Integer(istr_mant.argumento[1]), &
//											Integer(istr_mant.argumento[2]))
//
//IF ll_Filas = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF ll_Filas = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa
//	vinf.dw_1.Object.dw_detalle.Object.rut_empresa.text = 'R.U.T. ' + String(Double(Mid(gstr_apl.rut_empresa,1,9)),'###,###,###') + '-' + Mid(gstr_apl.rut_empresa,10,1)
//	vinf.dw_1.Object.dw_detalle.Object.dir_empresa.text	= gstr_apl.dir_empresa
//	
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event resize;Integer		li_posi_y, li_objeto

tab_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - tab_1.y - 75)

tab_1.tabpage_1.dw_cam01.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_2.dw_cam02.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_3.dw_cam03.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_4.dw_cam04.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_5.dw_cam05.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_6.dw_cam06.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_7.dw_cam07.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_8.dw_cam08.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_9.dw_cam09.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_10.dw_cam10.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_11.dw_cam11.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_12.dw_cam12.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_13.dw_cam13.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_14.dw_cam14.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_15.dw_cam15.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_16.dw_cam16.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_17.dw_cam17.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_18.dw_cam18.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_19.dw_cam19.Resize(tab_1.Width - 59, tab_1.Height - 148)
tab_1.tabpage_20.dw_cam20.Resize(tab_1.Width - 59, tab_1.Height - 148)

tab_1.x					= 78
st_encabe.Width		= tab_1.Width

pb_lectura.x			= This.WorkSpaceWidth() - 292
pb_lectura.y			= 300
pb_lectura.Width		= 233
pb_lectura.height		= 196


pb_nuevo.x				= This.WorkSpaceWidth() - 292
pb_nuevo.Width			= 233
pb_nuevo.height		= 196

pb_insertar.x			= This.WorkSpaceWidth() - 292
pb_insertar.Width		= 233
pb_insertar.height	= 196

pb_eliminar.x			= This.WorkSpaceWidth() - 292
pb_eliminar.Width		= 233
pb_eliminar.height	= 196

pb_grabar.x				= This.WorkSpaceWidth() - 292
pb_grabar.Width		= 233
pb_grabar.height		= 196

pb_imprimir.x			= This.WorkSpaceWidth() - 292
pb_imprimir.Width		= 233
pb_imprimir.height	= 196

//li_posi_y	= gb_2.y - 92

IF pb_nuevo.Visible THEN
	li_objeto	++
	li_posi_y	+= 195
	pb_nuevo.y	= li_posi_y
END IF

IF pb_insertar.Visible THEN
	li_objeto	++
	li_posi_y	+= 195
	pb_insertar.y	= li_posi_y
END IF

IF pb_eliminar.Visible THEN
	li_objeto	++
	li_posi_y	+= 195
	pb_eliminar.y	= li_posi_y
END IF

IF pb_grabar.Visible THEN
	li_objeto	++
	li_posi_y	+= 195
	pb_grabar.y		= li_posi_y
END IF

IF pb_imprimir.Visible THEN
	li_objeto	++
	li_posi_y	+= 195
	pb_imprimir.y	= li_posi_y
END IF

pb_salir.x				= This.WorkSpaceWidth() - 292
pb_salir.y				= 1300
pb_salir.Width			= 233
pb_salir.height		= 196
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ordencargapallet
integer width = 3017
integer height = 336
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 428
integer taborder = 80
end type

event pb_nuevo::clicked;call super::clicked;//il_fila						=	0
//ii_nuevo						=	0
//ddlb_tipocond.Enabled	=	True
//dw_cliente.Enabled		=	True
//dw_destino.Enabled		=	True
//dw_plantadesp.Enabled	=	True
//em_fecha.Enabled			=	True
//em_numero.Enabled			=	True
//
//pb_imprimir.Enabled		=	False
//pb_eliminar.Enabled		=	False
//pb_grabar.Enabled			=	False
//pb_insertar.Enabled		=	False
//
//dw_cliente.SetFocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 132
integer taborder = 70
end type

event pb_lectura::clicked;call super::clicked;//ddlb_tipocond.Enabled	=	False
//dw_cliente.Enabled		=	False
//dw_destino.Enabled		=	False
//dw_plantadesp.Enabled	=	False
//em_fecha.Enabled			=	False
//em_numero.Enabled			=	False
//
//dw_1.SetFocus()
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 788
integer taborder = 110
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 608
integer taborder = 100
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 1532
integer taborder = 140
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 1148
integer taborder = 130
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ordencargapallet
integer x = 3186
integer y = 968
integer taborder = 120
end type

event pb_grabar::clicked;call super::clicked;Integer li_fila, li_pallet

li_fila=dw_1.rowcount()

if li_fila>0 then
	li_pallet = dw_1.getitemnumber(li_fila, "paen_numero")
end if
	
	
end event

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ordencargapallet
boolean visible = false
integer y = 472
integer width = 3017
integer height = 1276
integer taborder = 90
boolean enabled = false
string dataobject = "dw_mues_ordencargapallet"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Long		ll_null

SetNull(ll_null)

dw_1.SetFocus()
dw_1.SetColumn(1)

ls_columna	=	dwo.Name
 CHOOSE CASE ls_columna
 	CASE "paen_numero"
		IF Duplicado(data) THEN
			This.SetItem(row,ls_columna,ll_null)			
			RETURN 1	
		ELSEIF NoExistePallet(data) THEN			
			This.SetItem(row,ls_columna,ll_null)			
   		RETURN 1
		END IF
 END CHOOSE

end event

type tab_1 from tab within w_mant_mues_ordencargapallet
event create ( )
event destroy ( )
integer x = 78
integer y = 472
integer width = 3058
integer height = 1340
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
boolean createondemand = true
alignment alignment = center!
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
tabpage_6 tabpage_6
tabpage_7 tabpage_7
tabpage_8 tabpage_8
tabpage_9 tabpage_9
tabpage_10 tabpage_10
tabpage_11 tabpage_11
tabpage_12 tabpage_12
tabpage_13 tabpage_13
tabpage_14 tabpage_14
tabpage_15 tabpage_15
tabpage_16 tabpage_16
tabpage_17 tabpage_17
tabpage_18 tabpage_18
tabpage_19 tabpage_19
tabpage_20 tabpage_20
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.tabpage_5=create tabpage_5
this.tabpage_6=create tabpage_6
this.tabpage_7=create tabpage_7
this.tabpage_8=create tabpage_8
this.tabpage_9=create tabpage_9
this.tabpage_10=create tabpage_10
this.tabpage_11=create tabpage_11
this.tabpage_12=create tabpage_12
this.tabpage_13=create tabpage_13
this.tabpage_14=create tabpage_14
this.tabpage_15=create tabpage_15
this.tabpage_16=create tabpage_16
this.tabpage_17=create tabpage_17
this.tabpage_18=create tabpage_18
this.tabpage_19=create tabpage_19
this.tabpage_20=create tabpage_20
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4,&
this.tabpage_5,&
this.tabpage_6,&
this.tabpage_7,&
this.tabpage_8,&
this.tabpage_9,&
this.tabpage_10,&
this.tabpage_11,&
this.tabpage_12,&
this.tabpage_13,&
this.tabpage_14,&
this.tabpage_15,&
this.tabpage_16,&
this.tabpage_17,&
this.tabpage_18,&
this.tabpage_19,&
this.tabpage_20}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
destroy(this.tabpage_5)
destroy(this.tabpage_6)
destroy(this.tabpage_7)
destroy(this.tabpage_8)
destroy(this.tabpage_9)
destroy(this.tabpage_10)
destroy(this.tabpage_11)
destroy(this.tabpage_12)
destroy(this.tabpage_13)
destroy(this.tabpage_14)
destroy(this.tabpage_15)
destroy(this.tabpage_16)
destroy(this.tabpage_17)
destroy(this.tabpage_18)
destroy(this.tabpage_19)
destroy(this.tabpage_20)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
long backcolor = 12632256
string text = "Cámara 1"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam01 dw_cam01
end type

on tabpage_1.create
this.dw_cam01=create dw_cam01
this.Control[]={this.dw_cam01}
end on

on tabpage_1.destroy
destroy(this.dw_cam01)
end on

type dw_cam01 from datawindow within tabpage_1
integer x = 14
integer y = 16
integer width = 2999
integer height = 1192
integer taborder = 10
string title = "none"
string dataobject = "dw_cons_ordencargapallet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_2 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 2"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam02 dw_cam02
end type

on tabpage_2.create
this.dw_cam02=create dw_cam02
this.Control[]={this.dw_cam02}
end on

on tabpage_2.destroy
destroy(this.dw_cam02)
end on

type dw_cam02 from datawindow within tabpage_2
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_3 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 3"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam03 dw_cam03
end type

on tabpage_3.create
this.dw_cam03=create dw_cam03
this.Control[]={this.dw_cam03}
end on

on tabpage_3.destroy
destroy(this.dw_cam03)
end on

type dw_cam03 from datawindow within tabpage_3
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_4 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 4"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam04 dw_cam04
end type

on tabpage_4.create
this.dw_cam04=create dw_cam04
this.Control[]={this.dw_cam04}
end on

on tabpage_4.destroy
destroy(this.dw_cam04)
end on

type dw_cam04 from datawindow within tabpage_4
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_5 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 5"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam05 dw_cam05
end type

on tabpage_5.create
this.dw_cam05=create dw_cam05
this.Control[]={this.dw_cam05}
end on

on tabpage_5.destroy
destroy(this.dw_cam05)
end on

type dw_cam05 from datawindow within tabpage_5
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_6 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 6"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam06 dw_cam06
end type

on tabpage_6.create
this.dw_cam06=create dw_cam06
this.Control[]={this.dw_cam06}
end on

on tabpage_6.destroy
destroy(this.dw_cam06)
end on

type dw_cam06 from datawindow within tabpage_6
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_7 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 7"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam07 dw_cam07
end type

on tabpage_7.create
this.dw_cam07=create dw_cam07
this.Control[]={this.dw_cam07}
end on

on tabpage_7.destroy
destroy(this.dw_cam07)
end on

type dw_cam07 from datawindow within tabpage_7
integer x = 14
integer y = 16
integer width = 2990
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_8 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 8"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam08 dw_cam08
end type

on tabpage_8.create
this.dw_cam08=create dw_cam08
this.Control[]={this.dw_cam08}
end on

on tabpage_8.destroy
destroy(this.dw_cam08)
end on

type dw_cam08 from datawindow within tabpage_8
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_9 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 9"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam09 dw_cam09
end type

on tabpage_9.create
this.dw_cam09=create dw_cam09
this.Control[]={this.dw_cam09}
end on

on tabpage_9.destroy
destroy(this.dw_cam09)
end on

type dw_cam09 from datawindow within tabpage_9
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_10 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 10"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam10 dw_cam10
end type

on tabpage_10.create
this.dw_cam10=create dw_cam10
this.Control[]={this.dw_cam10}
end on

on tabpage_10.destroy
destroy(this.dw_cam10)
end on

type dw_cam10 from datawindow within tabpage_10
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_11 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 11"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam11 dw_cam11
end type

on tabpage_11.create
this.dw_cam11=create dw_cam11
this.Control[]={this.dw_cam11}
end on

on tabpage_11.destroy
destroy(this.dw_cam11)
end on

type dw_cam11 from datawindow within tabpage_11
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_12 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 12"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam12 dw_cam12
end type

on tabpage_12.create
this.dw_cam12=create dw_cam12
this.Control[]={this.dw_cam12}
end on

on tabpage_12.destroy
destroy(this.dw_cam12)
end on

type dw_cam12 from datawindow within tabpage_12
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_13 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 13"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam13 dw_cam13
end type

on tabpage_13.create
this.dw_cam13=create dw_cam13
this.Control[]={this.dw_cam13}
end on

on tabpage_13.destroy
destroy(this.dw_cam13)
end on

type dw_cam13 from datawindow within tabpage_13
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_14 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 14"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam14 dw_cam14
end type

on tabpage_14.create
this.dw_cam14=create dw_cam14
this.Control[]={this.dw_cam14}
end on

on tabpage_14.destroy
destroy(this.dw_cam14)
end on

type dw_cam14 from datawindow within tabpage_14
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_15 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 15"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam15 dw_cam15
end type

on tabpage_15.create
this.dw_cam15=create dw_cam15
this.Control[]={this.dw_cam15}
end on

on tabpage_15.destroy
destroy(this.dw_cam15)
end on

type dw_cam15 from datawindow within tabpage_15
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_16 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 16"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam16 dw_cam16
end type

on tabpage_16.create
this.dw_cam16=create dw_cam16
this.Control[]={this.dw_cam16}
end on

on tabpage_16.destroy
destroy(this.dw_cam16)
end on

type dw_cam16 from datawindow within tabpage_16
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_17 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 17"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam17 dw_cam17
end type

on tabpage_17.create
this.dw_cam17=create dw_cam17
this.Control[]={this.dw_cam17}
end on

on tabpage_17.destroy
destroy(this.dw_cam17)
end on

type dw_cam17 from datawindow within tabpage_17
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_18 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 18"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam18 dw_cam18
end type

on tabpage_18.create
this.dw_cam18=create dw_cam18
this.Control[]={this.dw_cam18}
end on

on tabpage_18.destroy
destroy(this.dw_cam18)
end on

type dw_cam18 from datawindow within tabpage_18
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_19 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 19"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam19 dw_cam19
end type

on tabpage_19.create
this.dw_cam19=create dw_cam19
this.Control[]={this.dw_cam19}
end on

on tabpage_19.destroy
destroy(this.dw_cam19)
end on

type dw_cam19 from datawindow within tabpage_19
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type tabpage_20 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 104
integer width = 3022
integer height = 1220
boolean enabled = false
long backcolor = 12632256
string text = "Cámara 20"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_cam20 dw_cam20
end type

on tabpage_20.create
this.dw_cam20=create dw_cam20
this.Control[]={this.dw_cam20}
end on

on tabpage_20.destroy
destroy(this.dw_cam20)
end on

type dw_cam20 from datawindow within tabpage_20
integer x = 14
integer y = 16
integer width = 2953
integer height = 1128
integer taborder = 20
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;is_Columna	=	dwo.Name

IF Row > 0 THEN
	IF is_Columna <> "datawindow" THEN
		IF This.GetItemNumber(Row, is_Columna) > 0 THEN
			ii_Base		=	This.Object.cla1_nrbase[Row]
			ii_Altura	=	This.Object.cla1_nraltu[Row]
			ii_Calle		=	Integer(Mid(is_Columna, 6, 2))
			is_Estado	=	"estado" + String(ii_Calle)
			ii_Estado	=	Mod(This.GetItemNumber(Row, is_Estado) + 1, 3)
			
			This.SetItem(Row, is_Estado, ii_Estado)
		END IF		
	END IF
END IF

RETURN 0
end event

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_ordencargapallet
event destroy ( )
integer x = 686
integer y = 104
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;IF Not IsNull(This.Codigo) THEN
//	iuo_Planta.Existe(gi_CodExport, This.Codigo, False, sqlca)
//	iuo_Planta.Camaras(This.Codigo, sqlca)
	
	IF iuo_Planta.CantCamaras >= 1 THEN &
		HabilitaCamara(1, tab_1.tabpage_1, tab_1.tabpage_1.dw_cam01)
	
	IF iuo_Planta.CantCamaras >= 2 THEN &
		HabilitaCamara(2, tab_1.tabpage_2, tab_1.tabpage_2.dw_cam02)
	
	IF iuo_Planta.CantCamaras >= 3 THEN &
		HabilitaCamara(3, tab_1.tabpage_3, tab_1.tabpage_3.dw_cam03)
	
	IF iuo_Planta.CantCamaras >= 4 THEN &
		HabilitaCamara(4, tab_1.tabpage_4, tab_1.tabpage_4.dw_cam04)
	
	IF iuo_Planta.CantCamaras >= 5 THEN &
		HabilitaCamara(5, tab_1.tabpage_5, tab_1.tabpage_5.dw_cam05)
	
	IF iuo_Planta.CantCamaras >= 6 THEN &
		HabilitaCamara(6, tab_1.tabpage_6, tab_1.tabpage_6.dw_cam06)
	
	IF iuo_Planta.CantCamaras >= 7 THEN &
		HabilitaCamara(7, tab_1.tabpage_7, tab_1.tabpage_7.dw_cam07)
	
	IF iuo_Planta.CantCamaras >= 8 THEN &
		HabilitaCamara(8, tab_1.tabpage_8, tab_1.tabpage_8.dw_cam08)
	
	IF iuo_Planta.CantCamaras >= 9 THEN &
		HabilitaCamara(9, tab_1.tabpage_9, tab_1.tabpage_9.dw_cam09)
	
	IF iuo_Planta.CantCamaras >= 10 THEN &
		HabilitaCamara(10, tab_1.tabpage_10, tab_1.tabpage_10.dw_cam10)
	
	IF iuo_Planta.CantCamaras >= 11 THEN &
		HabilitaCamara(11, tab_1.tabpage_11, tab_1.tabpage_11.dw_cam11)
	
	IF iuo_Planta.CantCamaras >= 12 THEN &
		HabilitaCamara(12, tab_1.tabpage_12, tab_1.tabpage_12.dw_cam12)
	
	IF iuo_Planta.CantCamaras >= 13 THEN &
		HabilitaCamara(13, tab_1.tabpage_13, tab_1.tabpage_13.dw_cam13)
	
	IF iuo_Planta.CantCamaras >= 14 THEN &
		HabilitaCamara(14, tab_1.tabpage_14, tab_1.tabpage_14.dw_cam14)
	
	IF iuo_Planta.CantCamaras >= 15 THEN &
		HabilitaCamara(15, tab_1.tabpage_15, tab_1.tabpage_15.dw_cam15)
	
	IF iuo_Planta.CantCamaras >= 16 THEN &
		HabilitaCamara(16, tab_1.tabpage_16, tab_1.tabpage_16.dw_cam16)
	
	IF iuo_Planta.CantCamaras >= 17 THEN &
		HabilitaCamara(17, tab_1.tabpage_17, tab_1.tabpage_17.dw_cam17)
	
	IF iuo_Planta.CantCamaras >= 18 THEN &
		HabilitaCamara(18, tab_1.tabpage_18, tab_1.tabpage_18.dw_cam18)
	
	IF iuo_Planta.CantCamaras >= 19 THEN &
		HabilitaCamara(19, tab_1.tabpage_19, tab_1.tabpage_19.dw_cam19)
	
	IF iuo_Planta.CantCamaras >= 20 THEN &
		HabilitaCamara(20, tab_1.tabpage_20, tab_1.tabpage_20.dw_cam20)

END IF
end event

type st_planta from statictext within w_mant_mues_ordencargapallet
integer x = 160
integer y = 112
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type st_1 from statictext within w_mant_mues_ordencargapallet
integer x = 160
integer y = 212
integer width = 494
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Tipo de Transporte"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_ordencargapallet
integer x = 169
integer y = 308
integer width = 512
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Orden de Embarque"
boolean focusrectangle = false
end type

type uo_seltransporte from uo_seleccion_tipotransporte within w_mant_mues_ordencargapallet
integer x = 686
integer y = 200
integer width = 0
integer height = 80
integer taborder = 80
boolean bringtotop = true
long backcolor = 0
string text = ""
long tabtextcolor = 0
long picturemaskcolor = 0
end type

on uo_seltransporte.destroy
call uo_seleccion_tipotransporte::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE "*", "**"
		uo_SelOrden.Todos(True)
		
		uo_SelOrden.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelOrden.TipoTransporte		=	This.Codigo
		uo_SelOrden.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type uo_selorden from uo_seleccion_oredencarga within w_mant_mues_ordencargapallet
integer x = 686
integer y = 296
integer height = 80
integer taborder = 90
boolean bringtotop = true
end type

on uo_selorden.destroy
call uo_seleccion_oredencarga::destroy
end on

