$PBExportHeader$w_mant_mues_inspecpaldet_de_rechazo_nuevo.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_mues_inspecpaldet_de_rechazo_nuevo from w_mant_directo
end type
type em_numero from editmask within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_4 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_plantadesp from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_2 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_cliente from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_1 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_3 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_destino from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_5 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_6 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type em_fecha from editmask within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_2 from uo_dw within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_7 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type em_origen from editmask within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_3 from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_4 from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type dw_descripcion from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type st_8 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type sle_detalle from singlelineedit within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
type cbx_facturable from checkbox within w_mant_mues_inspecpaldet_de_rechazo_nuevo
end type
end forward

global type w_mant_mues_inspecpaldet_de_rechazo_nuevo from w_mant_directo
integer x = 155
integer y = 156
integer width = 3639
string title = "DETALLE DE PALLET PARA INSPECCION / RE-INSPECCION"
event ue_validaborrar ( )
em_numero em_numero
ddlb_tipocond ddlb_tipocond
st_4 st_4
dw_plantadesp dw_plantadesp
st_2 st_2
dw_cliente dw_cliente
st_1 st_1
st_3 st_3
dw_destino dw_destino
st_5 st_5
st_6 st_6
em_fecha em_fecha
dw_2 dw_2
st_7 st_7
em_origen em_origen
dw_3 dw_3
dw_4 dw_4
dw_descripcion dw_descripcion
st_8 st_8
sle_detalle sle_detalle
cbx_facturable cbx_facturable
end type
global w_mant_mues_inspecpaldet_de_rechazo_nuevo w_mant_mues_inspecpaldet_de_rechazo_nuevo

type variables
DataWindowChild	dwc_plantas, dwc_mercado, dwc_especie, dwc_variedad, &
						dwc_embalaje, dwc_tipopalemb, dwc_descripcion

Integer  ii_pallet, ii_nuevo, ii_especie
String	is_descripcion
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean duplicado (string as_pallet)
public function boolean noexistepallet (string as_pallet)
public function boolean noexistepallet_ii ()
public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia)
end prototypes

event ue_validaborrar;Integer li_estado

IF dw_1.GetRow()>0 THEN
	li_estado	=	dw_1.GetItemNumber(dw_1.GetRow(),"paen_estado")
	
	IF li_estado=2 THEN
		MessageBox("Error", "Pallet fue Despachado, no se puede eliminar " + &
						"de esta inspección.~r~rIngrese o seleccione otro Pallet.")
						
		Message.DoubleParm	=	-1
	ELSEIF li_estado=3 THEN
		MessageBox("Error", "Pallet fue Repalletizado, no se puede eliminar " + &
						"de esta inspección.~r~rIngrese o seleccione otro Pallet.")
						
		Message.DoubleParm	=	-1
	END IF
END IF
end event

protected function boolean wf_actualiza_db ();//IF Not dw_3.uf_check_required(0) THEN RETURN False

//IF Not dw_3.uf_validate(0) THEN RETURN False

IF dw_4.Update() = 1 AND dw_3.Update() = 1 THEN
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RETURN False		
	ELSE
		
		RETURN True
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	RETURN False
END IF

RETURN True
end function

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
	FROM	dbo.palletencab as pae, dbo.especies as esp,
			dbo.variedades as var, dbo.embalajesprod as emb
	WHERE pae.clie_codigo	= :li_cliente
	AND	pae.paen_numero	= :ll_numero
	AND	pae.plde_codigo	= :li_planta
	AND	esp.espe_codigo	= pae.espe_codigo
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
		FROM	dbo.inspecpaldet
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
		dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
		dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
		dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
		dw_1.Object.dest_codigo[il_fila] = dw_destino.Object.dest_codigo[1]
		RETURN False
	ELSE
		RETURN True
	END IF
ELSE	
	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
	dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
	dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
	dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
	dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
	dw_1.Object.dest_codigo[il_fila] = dw_destino.Object.dest_codigo[1]
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
	FROM	dbo.palletencab as pae, dbo.especies as esp,
			dbo.variedades as var, dbo.embalajesprod as emb
	WHERE pae.clie_codigo	= :li_cliente
	AND	pae.paen_numero	= :ll_numero
	AND	pae.plde_codigo	= :li_planta
	AND	esp.espe_codigo	= pae.espe_codigo
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
		FROM	dbo.inspecpaldet
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
		dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
		dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
		dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
		dw_1.Object.dest_codigo[il_fila] = dw_destino.Object.dest_codigo[1]
		RETURN False
	ELSE
		RETURN True
	END IF
ELSE	
	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
	dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
	dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
	dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
	dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
	dw_1.Object.dest_codigo[il_fila] = dw_destino.Object.dest_codigo[1]
	RETURN False
END IF

end function

public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia);Integer	li_Cliente, li_Planta
Long		ll_Numero
String	ls

SELECT dsag_descrip
INTO :is_descripcion
FROM dbo.destinossag
WHERE dest_codigo	=	:ai_destino
AND	espe_codigo =  :ai_especie
AND	dsag_secuen =  :ai_secuencia;

IF is_descripcion = '' THEN
	MessageBox("Cuidado","destinossag No Existe")
	RETURN False
END IF
RETURN True
end function

event open;call super::open;dw_cliente.SetTransObject(Sqlca)

dw_destino.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(-1)

dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)

dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

dw_2.GetChild("espe_codigo",dwc_especie)
dw_2.GetChild("vari_codigo",dwc_variedad)
dw_2.GetChild("emba_codigo",dwc_embalaje)
dw_2.GetChild("tpem_codigo",dwc_tipopalemb)

dwc_especie.SetTransObject(Sqlca)
dwc_variedad.SetTransObject(Sqlca)
dwc_embalaje.SetTransObject(Sqlca)
dwc_tipopalemb.SetTransObject(Sqlca)

dwc_especie.Retrieve()				//Especies del Cliente
dwc_variedad.Retrieve(0)			//Variedades del Cliente y la Especie
dwc_embalaje.Retrieve(gi_CodExport)				//Embalajes del Cliente
dwc_tipopalemb.Retrieve(gi_CodExport, 'Z')	//Tipos de Pallet por Embalajes del Cliente

dw_1.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(0)

dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
dwc_descripcion.SetTransObject(Sqlca)
dwc_descripcion.Retrieve(gi_CodEspecie,-1)			
dw_descripcion.InsertRow(1)

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)

dw_3.Reset()
dw_4.Reset()

ddlb_tipocond.SelectItem(1)

em_fecha.Text				=	String(Today())
buscar						=	"Pallet:Npaen_numero,Especie:Sespe_nombre," + &
									"Variedad:Svari_nombre,Embalaje:Semba_nombre," + &
									"Cajas:Npaen_ccajas"
ordenar						=	"Pallet:paen_numero,Especie:espe_nombre," + &
									"Variedad:vari_nombre,Embalaje:emba_nombre," + &
									"Cajas:paen_ccajas"
is_ultimacol				=	"paen_numero"
istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)
istr_mant.Argumento[3]	=	"1"
istr_mant.Argumento[6]	=	String(Today())

end event

on w_mant_mues_inspecpaldet_de_rechazo_nuevo.create
int iCurrent
call super::create
this.em_numero=create em_numero
this.ddlb_tipocond=create ddlb_tipocond
this.st_4=create st_4
this.dw_plantadesp=create dw_plantadesp
this.st_2=create st_2
this.dw_cliente=create dw_cliente
this.st_1=create st_1
this.st_3=create st_3
this.dw_destino=create dw_destino
this.st_5=create st_5
this.st_6=create st_6
this.em_fecha=create em_fecha
this.dw_2=create dw_2
this.st_7=create st_7
this.em_origen=create em_origen
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_descripcion=create dw_descripcion
this.st_8=create st_8
this.sle_detalle=create sle_detalle
this.cbx_facturable=create cbx_facturable
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_numero
this.Control[iCurrent+2]=this.ddlb_tipocond
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.dw_cliente
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.dw_destino
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.em_fecha
this.Control[iCurrent+13]=this.dw_2
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.em_origen
this.Control[iCurrent+16]=this.dw_3
this.Control[iCurrent+17]=this.dw_4
this.Control[iCurrent+18]=this.dw_descripcion
this.Control[iCurrent+19]=this.st_8
this.Control[iCurrent+20]=this.sle_detalle
this.Control[iCurrent+21]=this.cbx_facturable
end on

on w_mant_mues_inspecpaldet_de_rechazo_nuevo.destroy
call super::destroy
destroy(this.em_numero)
destroy(this.ddlb_tipocond)
destroy(this.st_4)
destroy(this.dw_plantadesp)
destroy(this.st_2)
destroy(this.dw_cliente)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.dw_destino)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.em_fecha)
destroy(this.dw_2)
destroy(this.st_7)
destroy(this.em_origen)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_descripcion)
destroy(this.st_8)
destroy(this.sle_detalle)
destroy(this.cbx_facturable)
end on

event ue_antesguardar;Long	ll_fila = 1
Integer	li_factur

DO WHILE ll_fila <= dw_1.RowCount()
	dw_3.Object.inpe_tipoin[ll_fila] = Integer(istr_mant.argumento[3])
	dw_3.Object.inpe_numero[ll_fila] = Long(istr_mant.argumento[4])
	dw_3.Object.clie_codigo[ll_fila] = Integer(istr_mant.argumento[1])
	dw_3.Object.plde_codigo[ll_fila] = Integer(istr_mant.argumento[2])
	dw_3.Object.inpe_secuen[ll_fila] = 1
	dw_3.Object.inpd_fechai[ll_fila] = Date(istr_mant.argumento[6])
	dw_3.Object.paen_numero[ll_fila] = dw_1.Object.paen_numero[ll_fila]
	dw_3.Object.inpd_nroanu[ll_fila] = dw_1.Object.inpd_nroanu[ll_fila]
	dw_3.Object.inpd_fechaa[ll_fila] = dw_1.Object.inpd_fechaa[ll_fila]
	dw_3.Object.dest_codigo[ll_fila] = Integer(istr_mant.argumento[5])
	dw_3.Object.inpd_ccajas[ll_fila] = dw_1.Object.inpd_ccajas[ll_fila]
	ll_fila ++
LOOP

ll_fila=1

IF cbx_facturable.Checked THEN
	li_factur = 1
ELSe
	li_factur = 0
END IF

DO WHILE ll_fila <= dw_2.RowCount()
	dw_4.Object.inpe_tipoin[1]	=	Integer(istr_mant.argumento[3])
	dw_4.Object.inpe_numero[1]	=	Long(istr_mant.argumento[4])
	dw_4.Object.clie_codigo[1]	=	Integer(istr_mant.argumento[1])
	dw_4.Object.plde_codigo[1]	=	Integer(istr_mant.argumento[2])
	dw_4.Object.inpe_secuen[1]	=	1
	dw_4.Object.dest_codigo[1]	=	Integer(istr_mant.argumento[5])
	dw_4.Object.inpe_fechai[1]	=	Date(istr_mant.argumento[6])
	dw_4.Object.espe_codigo[1]	=	dw_2.Object.espe_codigo[1]
	dw_4.Object.vari_codigo[1]	=	dw_2.Object.vari_codigo[1]
	dw_4.Object.emba_codigo[1]	=	dw_2.Object.emba_codigo[1]
	dw_4.Object.tpem_codigo[1]	=	dw_2.Object.tpem_codigo[1]
	dw_4.Object.inpe_calibr[1]	=	dw_2.Object.inpe_calibr[1]
	dw_4.Object.inpe_estado[1]	=	5
	dw_4.Object.inpe_desdet[1]	=	sle_detalle.Text
	dw_4.Object.inpe_factur[1]	=	li_factur
	dw_4.Object.inpe_dessec[1]	=	dw_descripcion.Object.dsag_secuen[1]
	dw_4.Object.inpe_desant[1]	=	dw_2.Object.inpe_desant[1]
	dw_4.Object.inpe_secant[1]	=	dw_2.Object.inpe_secant[1]
	dw_4.Object.inpe_detant[1]	=	dw_2.Object.inpe_detant[1]

	ll_fila ++
LOOP

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, ll_fila1, respuesta, ll_cont

ii_nuevo	=	0

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[1]),&
	                         Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[5]))
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		ll_fila1	=	dw_2.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
									 Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
		IF ll_fila1 > 0 THEN ii_nuevo	=	1
		
		FOR ll_cont = 1 TO dw_1.Rowcount()
			dw_1.Object.dest_codigo[ll_cont] = dw_destino.Object.dest_codigo[1]
		NEXT	
		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_eliminar.Enabled	=	True
		pb_grabar.Enabled		=	True
		pb_imprimir.Enabled	=	True
		
		istr_mant.argumento[4] = istr_mant.argumento[14]
		
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

lstr_info.titulo	= "SOLICITUD INSPECCION FITOSANITARIA S.A.G."
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_compuesto"

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
											Long(istr_mant.argumento[4]), &
											Integer(istr_mant.argumento[1]), &
											Integer(istr_mant.argumento[2]))

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
//	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa
//	vinf.dw_1.Object.dw_detalle.Object.rut_empresa.text = 'R.U.T. ' + String(Double(Mid(gstr_apl.rut_empresa,1,9)),'###,###,###') + '-' + Mid(gstr_apl.rut_empresa,10,1)
//	vinf.dw_1.Object.dw_detalle.Object.dir_empresa.text	= gstr_apl.dir_empresa
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer y = 40
integer width = 3017
integer height = 568
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 3191
integer y = 528
integer taborder = 100
end type

event pb_nuevo::clicked;call super::clicked;Integer	li_null

SetNull(li_null)

dw_2.Reset()
dw_3.Reset()
dw_4.Reset()

il_fila						=	0
ii_nuevo						=	0
ddlb_tipocond.Enabled	=	True
dw_cliente.Enabled		=	True
dw_destino.Enabled		=	True
dw_plantadesp.Enabled	=	True
em_fecha.Enabled			=	True
em_numero.Enabled			=	True
em_origen.Enabled			=	True

pb_imprimir.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_grabar.Enabled			=	False
pb_insertar.Enabled		=	False
dw_descripcion.Enabled  =  False

dw_destino.Object.dest_codigo[1] 		= li_null
dw_descripcion.Object.dsag_secuen[1] 	= li_null
em_numero.Text									=	''
em_origen.Text									=	''
sle_detalle.Text								=	''

dw_cliente.SetFocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 3191
integer y = 128
integer taborder = 90
end type

event pb_lectura::clicked;IF em_numero.Text = '' THEN
	MessageBox("Atención","Falta Ingresar Número de Inspección")
	Return 1
END IF	

IF dw_descripcion.Object.dsag_secuen[1] = 0 OR Isnull(dw_descripcion.Object.dsag_secuen[1]) THEN
	MessageBox("Advertencia","Falta Ingreso Secuencia Destino.")
	Message.DoubleParm = -1
	Return
END IF	

Parent.PostEvent("ue_recuperadatos")

ddlb_tipocond.Enabled	=	False
dw_cliente.Enabled		=	False
dw_destino.Enabled		=	False
dw_plantadesp.Enabled	=	False
em_fecha.Enabled			=	False
em_numero.Enabled			=	False
em_origen.Enabled			=	False
dw_descripcion.Enabled  =  False

dw_1.SetFocus()
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 3186
integer y = 912
integer taborder = 130
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 3186
integer y = 732
integer taborder = 120
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 3186
integer y = 1588
integer taborder = 160
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_inspecpaldet_de_rechazo_nuevo
boolean visible = false
integer x = 3186
integer y = 1272
integer taborder = 150
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 3186
integer y = 1092
integer taborder = 140
end type

event pb_grabar::clicked;call super::clicked;Integer li_fila, li_pallet

li_fila=dw_1.rowcount()

if li_fila>0 then
	li_pallet = dw_1.getitemnumber(li_fila, "paen_numero")
end if
	
	
end event

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer y = 624
integer width = 3017
integer height = 1216
integer taborder = 110
string dataobject = "dw_mues_inspecpaldet"
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

event dw_1::buttonclicked;call super::buttonclicked;str_busqueda	lstr_busq
Integer li_valor, li_estado


li_estado= This.getitemnumber(this.getrow(),"paen_numero")
li_valor=this.getitemnumber(this.getrow(),"paen_estado")
	if li_valor =0 or isnull(li_valor)=True then
		lstr_busq.argum[1]	=	istr_mant.argumento[1]
		lstr_busq.argum[5]	=	istr_mant.argumento[2]
		OpenWithParm(w_busc_palletencab, lstr_busq)
		lstr_busq	       = Message.PowerObjectParm
		IF lstr_busq.argum[2] <> "" THEN
			IF NOT Duplicado(lstr_busq.argum[2]) THEN	NoExistePallet(lstr_busq.argum[2])
		ELSE
			This.SetColumn("paen_numero")
		END IF
	else
		if li_estado = 1 then
			CHOOSE CASE dwo.Name
				CASE "b_pallet"
					lstr_busq.argum[1]	=	istr_mant.argumento[1]
					lstr_busq.argum[2]	=	""
					lstr_busq.argum[3]	=	"1"
					lstr_busq.argum[5]	=	istr_mant.argumento[2]
					
					
					IF istr_mant.argumento[3] = "1" THEN
						lstr_busq.Argum[4]	=	"0"
					ELSE
						lstr_busq.Argum[4]	=	"1"
					END IF
					
					OpenWithParm(w_busc_palletencab, lstr_busq)
					
					lstr_busq	       = Message.PowerObjectParm
					
					IF lstr_busq.argum[2] <> "" THEN
						IF NOT Duplicado(lstr_busq.argum[2]) THEN	NoExistePallet(lstr_busq.argum[2])
					ELSE
						This.SetColumn("paen_numero")
					END IF
			END CHOOSE
		else
			messagebox("Error","El Pallet ya fue Despachado o Repalletizado, no se puede modificar")
		end if
	end if
end event

type em_numero from editmask within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 2592
integer y = 64
integer width = 402
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long		ll_numero, ll_inpe_numero
Integer	li_planta, li_cliente, li_tipoin

ll_numero 	=	Long(em_numero.Text)
li_cliente	=	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])
li_tipoin	=	Integer(istr_mant.argumento[3])

istr_mant.argumento[14]	=	String(ll_numero)

SELECT Distinct inpe_numero
INTO   :ll_inpe_numero
FROM dbo.inspecpalenc
WHERE inpe_numero = :ll_numero
AND   clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND   inpe_tipoin = :li_tipoin;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Tabla de Inspección")
	em_numero.Text = ''
	em_numero.SetFocus()
ELSEIF sqlca.SQLCode = 0 THEN
	MessageBox("Atención", "Número de Inspección ya Existe.~rIngrese otro.", + &
	Exclamation!, OK!)
	em_numero.Text = ''
	em_numero.SetFocus()
END IF


end event

type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 471
integer y = 280
integer width = 526
integer height = 292
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
boolean sorted = false
string item[] = {"Inspección","Re-Inspección"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
end event

type st_4 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 114
integer y = 280
integer width = 334
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
string text = "Condición"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 471
integer y = 172
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	RETURN 1
END IF
end event

type st_2 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 114
integer y = 172
integer width = 347
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

type dw_cliente from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 471
integer y = 64
integer width = 1161
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	
	dwc_plantas.Retrieve(1)
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_1 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 114
integer y = 64
integer width = 347
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

type st_3 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 1682
integer y = 64
integer width = 443
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
string text = "Nuevo Número"
boolean focusrectangle = false
end type

type dw_destino from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 2135
integer y = 280
integer width = 882
integer height = 92
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)

ls_columna	= dwo.Name

CHOOSE CASE ls_columna
	CASE "dest_codigo"
		istr_mant.argumento[5]	=	data
		
		dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
		dwc_descripcion.SetTransObject(Sqlca)
		IF dwc_descripcion.Retrieve(ii_especie,integer(data)) = 0 THEN
			dw_descripcion.SetItem(1, "dsag_secuen", li_null)
			MessageBox("Atención", "No Existe Secuencia para ese Destino, Haga Mantención Tabla Destinos Sag.", &
			Exclamation!, OK!)
			dw_descripcion.SetItem(1, "dsag_secuen", li_null)
			This.SetItem(1, "dest_codigo", li_null)
			Return 
		END IF	
		dw_descripcion.Enabled = True
				
END CHOOSE
end event

type st_5 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 1682
integer y = 280
integer width = 430
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
string text = "Nuevo Destino"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 1024
integer y = 280
integer width = 215
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
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 1239
integer y = 276
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;IF Not f_validafechatempo(date(this.Text)) THEN
	This.Text = ''
	This.SetFocus()
END IF

istr_mant.argumento[6]	=	This.Text
end event

type dw_2 from uo_dw within w_mant_mues_inspecpaldet_de_rechazo_nuevo
boolean visible = false
integer x = 128
integer y = 896
integer width = 2885
integer height = 520
integer taborder = 80
boolean bringtotop = true
string dataobject = "dw_mues_inspecpalenc"
boolean hscrollbar = true
end type

type st_7 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 1682
integer y = 172
integer width = 498
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
string text = "Rechazo Origen "
boolean focusrectangle = false
end type

type em_origen from editmask within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 2592
integer y = 172
integer width = 402
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long		ll_numero, ll_secuencia
Integer	li_planta, li_cliente, li_tipoin, li_destino, li_especie
Date		ld_frecha, ld_fechai
String	ls_detdestino

ll_numero 	=	Long(em_origen.Text)
li_cliente	=	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])
li_tipoin	=	Integer(istr_mant.argumento[3])

istr_mant.argumento[4]	=	String(ll_numero)

SELECT max(inpe_secuen)
INTO   :ll_secuencia
FROM dbo.inspecpaldet
WHERE inpe_numero = :ll_numero
AND   clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND   inpe_tipoin = :li_tipoin;

SELECT DISTINCT inpd_frecha
INTO   :ld_frecha
FROM dbo.inspecpaldet
WHERE inpe_numero = :ll_numero
AND   clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND   inpe_tipoin = :li_tipoin
AND	inpe_secuen = :ll_secuencia;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Tabla de Inspección")
	em_origen.Text = ''
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Inspección NO Existe.~rIngrese otro.", + &
	Exclamation!, OK!)
	em_origen.Text = ''	
	
ELSEIF IsNull(ld_frecha) THEN
	MessageBox("Atención", "Número de Inspección No se ha Procesado en Rechazos SAG.~rIngrese otro.", + &
	Exclamation!, OK!)
	em_origen.Text = ''
	
ELSE
	SELECT DISTINCT inpe_fechai, dest_codigo, espe_codigo, inpe_desdet
	INTO   :ld_fechai, :li_destino, :ii_especie, :ls_detdestino
	FROM dbo.inspecpalenc
	WHERE inpe_numero = :ll_numero
	AND   clie_codigo = :li_cliente
	AND   plde_codigo = :li_planta
	AND   inpe_tipoin = :li_tipoin
	AND	inpe_secuen = :ll_secuencia;	

//	em_fecha.Text	=	String(Today())
//	ld_fechai		=	Date(em_fecha.Text)		
//	dw_destino.SetItem(1,"dest_codigo",li_destino)
//	istr_mant.argumento[5]	=	String(li_destino)
//	istr_mant.argumento[6]  =  String(ld_fechai)
//	sle_detalle.Text			=	ls_detdestino
//	
//	dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
//	dwc_descripcion.SetTransObject(Sqlca)
//	dwc_descripcion.Retrieve(ii_especie,li_destino)
//	dw_descripcion.Enabled = True
	
END IF
end event

type dw_3 from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
boolean visible = false
integer x = 288
integer y = 1396
integer width = 1161
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_inspecpaldet_gra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
boolean visible = false
integer x = 1541
integer y = 1404
integer width = 1513
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_inspecpalenc_gra_nuevo"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_descripcion from datawindow within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 2135
integer y = 384
integer width = 855
integer height = 92
integer taborder = 90
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinosag"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF existedescripcion(ii_especie,dw_destino.Object.dest_codigo[1],Integer(data)) THEN
	sle_detalle.Text = is_descripcion
END IF
end event

type st_8 from statictext within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 114
integer y = 496
integer width = 457
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
string text = "Det. Destino"
boolean focusrectangle = false
end type

type sle_detalle from singlelineedit within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 503
integer y = 488
integer width = 1847
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
integer limit = 600
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

type cbx_facturable from checkbox within w_mant_mues_inspecpaldet_de_rechazo_nuevo
integer x = 2377
integer y = 500
integer width = 681
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
string text = "Proceso Facturable  "
boolean checked = true
end type

