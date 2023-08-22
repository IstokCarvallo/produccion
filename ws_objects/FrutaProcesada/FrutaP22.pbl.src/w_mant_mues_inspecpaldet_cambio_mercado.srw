$PBExportHeader$w_mant_mues_inspecpaldet_cambio_mercado.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_mues_inspecpaldet_cambio_mercado from w_mant_directo
end type
type em_numero from editmask within w_mant_mues_inspecpaldet_cambio_mercado
end type
type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_4 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type dw_plantadesp from datawindow within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_2 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type dw_cliente from datawindow within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_1 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_3 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type dw_destino from datawindow within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_5 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_6 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type em_fecha from editmask within w_mant_mues_inspecpaldet_cambio_mercado
end type
type dw_2 from uo_dw within w_mant_mues_inspecpaldet_cambio_mercado
end type
type st_7 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
end type
type sle_solic from singlelineedit within w_mant_mues_inspecpaldet_cambio_mercado
end type
end forward

global type w_mant_mues_inspecpaldet_cambio_mercado from w_mant_directo
integer x = 155
integer y = 156
integer width = 3502
string title = "CAMBIO DE MERCADO"
event ue_validaborrar ( )
event ue_validapassword ( )
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
sle_solic sle_solic
end type
global w_mant_mues_inspecpaldet_cambio_mercado w_mant_mues_inspecpaldet_cambio_mercado

type variables
DataWindowChild	dwc_plantas, dwc_mercado, dwc_especie, dwc_variedad, &
						dwc_embalaje, dwc_tipopalemb

Integer ii_pallet, ii_nuevo

DataStore ids_palletencab
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean duplicado (string as_pallet)
public function boolean noexistepallet (string as_pallet)
public function boolean noexistepallet_ii ()
public function boolean noexisteinspeccion (integer ai_tipo, long al_numero, integer ai_planta, integer ai_cliente)
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

event ue_validapassword();Str_mant					lstr_mant
lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

protected function boolean wf_actualiza_db ();IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF dw_2.Update() = 1 AND dw_1.Update() = 1 AND ids_palletencab.Update() = 1 THEN
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
	   IF MessageBox("Atención","Pallet está FUMIGADO.~r~r" + &
			            "Desea continuar ?", Question!, YesNo!, 2) = 1 THEN 
          	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
				dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
				dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
				dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
				dw_1.SetItem(il_fila, "especies_espe_nombre", ls_especie)
				dw_1.SetItem(il_fila, "palletencab_paen_ccajas", ll_cajas)
				RETURN False
		ELSE
				RETURN True
		END IF						  
	
ELSEIF li_inspec=3 THEN /*Rechazado SAG*/
	      IF MessageBox("Advertencia", "Inspeccionará un Pallet Rechazado por SAG.~r~r" + &
			            "Desea continuar ?", Question!, YesNo!, 2) = 1 THEN 
          	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
				dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_variedad)
				dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
				dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
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
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", ls_embalaje)
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
	RETURN False
END IF

end function

public function boolean noexisteinspeccion (integer ai_tipo, long al_numero, integer ai_planta, integer ai_cliente);String	ls_Solnom
Integer	li_Planta,li_Destino, li_Cliente
Date     ld_fecha

  SELECT Distinct clie_codigo,plde_codigo,   
         dest_codigo,inpe_fechai,   
         inpe_solnom  
    INTO :li_Cliente,:li_Planta,
	      :li_Destino,
			:ld_Fecha,:ls_Solnom  
    FROM dbo.inspecpalenc  
   WHERE inpe_tipoin = :ai_tipo  AND  
         inpe_numero = :al_numero AND
			plde_codigo = :ai_planta AND
			clie_codigo	= :ai_cliente ;
				
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Inspecpalenc  ")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	/*MessageBox("Atención","Número de Inspección No Existe. Ingrese Otro.", Exclamation!, Ok!)*/
	RETURN True
ELSE
	dw_cliente.Object.clie_codigo[1]		= 	li_Cliente
	dw_plantadesp.Object.plde_codigo[1] =  li_Planta
	dw_destino.Object.dest_codigo[1]		=	li_Destino
	em_fecha.text								=	String(ld_Fecha)
	sle_solic.text								=	ls_Solnom
	
	istr_mant.argumento[1]  = String(li_Cliente)
	istr_mant.argumento[2]  = String(li_Planta)
	istr_mant.argumento[5]  = String(li_Destino)
	istr_mant.argumento[6]  = String(ld_fecha)
	istr_mant.argumento[7]  = ls_Solnom
	
		
	RETURN False
END IF


end function

event open;call super::open;PostEvent("ue_validapassword")

dw_cliente.SetTransObject(Sqlca)

dw_destino.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(-1)
dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)

dw_1.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(-1)


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



dw_2.SetTransObject(sqlca)
ddlb_tipocond.SelectItem(1)

ids_palletencab	=	Create DataStore
ids_palletencab.DataObject = "dw_mues_inspecpaldet_palletencab"
ids_palletencab.SetTransObject(sqlca)

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

on w_mant_mues_inspecpaldet_cambio_mercado.create
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
this.sle_solic=create sle_solic
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
this.Control[iCurrent+15]=this.sle_solic
end on

on w_mant_mues_inspecpaldet_cambio_mercado.destroy
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
destroy(this.sle_solic)
end on

event ue_antesguardar;Integer  li_destino
Long		ll_fila = 1, ll_fila1, ll_numero, total


DO WHILE ll_fila <= dw_1.RowCount()
	li_destino = dw_1.Object.dest_codigo[ll_fila]
	IF Isnull(li_destino) THEN li_destino = 0

	IF li_destino <> Integer(istr_mant.argumento[5]) THEN	
		dw_1.Object.dest_codigo[ll_fila]	=	Integer(istr_mant.argumento[5])
			
		ll_numero= dw_1.Object.paen_numero[ll_fila]
		total =ids_palletencab.rowcount()
		ll_fila1	= ids_palletencab.Find("paen_numero = " + String(ll_numero), 1, ids_palletencab.RowCount())

		IF ll_fila1 > 0 THEN			
			ids_palletencab.SetItem(ll_fila1, "paen_inspec", 1)
			ids_palletencab.SetItem(ll_fila1, "dest_codigo", Integer(istr_mant.argumento[5]))
		END IF
	END IF
	ll_fila ++

LOOP

IF ii_nuevo	= 1 THEN
	dw_2.Object.dest_codigo[1]	=	Integer(istr_mant.argumento[5])
END IF


end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, ll_fila1, ll_fila2, respuesta

ii_nuevo	=	0

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		IF ll_fila = 0 THEN
		    st_5.Visible			=	True
			 dw_destino.Enabled	=	True
			 dw_destino.Visible	=	True
		END IF
		
		ll_fila2	= ids_palletencab.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
										 Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
		IF ll_fila2 = 0 THEN
		    st_5.Visible			=	True
			 dw_destino.Enabled	=	True
			 dw_destino.Visible	=	True		
		ELSE
			ll_fila1	=	dw_2.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
											 Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
			IF ll_fila1 > 0 THEN ii_nuevo	=	1
			dw_1.SetRow(1)
			dw_1.SetFocus()
		END IF										 		 
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
Decimal	li_varrot, li_informe

str_info	lstr_info

lstr_info.titulo	= "SOLICITUD INSPECCION FITOSANITARIA S.A.G."
lstr_info.copias	= 1

li_varrot	=	0
li_informe	=	2

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_compuesto"

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
											Long(istr_mant.argumento[4]), &
											Integer(istr_mant.argumento[1]), &
											Integer(istr_mant.argumento[2]),li_varrot,li_informe,1,1,1)

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

event ue_listo;String	ls_Objetos, ls_Columna, ls_NumeroTab
Integer	li_Posicion

IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_Insertar.Enabled	=	False
		
		wf_BloqueaColumnas(True)
	ELSE
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	True
		pb_Insertar.Enabled	=	False
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_Insertar.Enabled	=	False
	ELSE
		pb_Insertar.Enabled	=	False
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_inspecpaldet_cambio_mercado
integer width = 3017
integer height = 532
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer y = 428
integer taborder = 100
end type

event pb_nuevo::clicked;call super::clicked;Integer  li_Null

SetNull(li_Null)

dw_2.Reset()
il_fila						=	0
ii_nuevo						=	0
ddlb_tipocond.Enabled	=	True
dw_cliente.Enabled		=	True
dw_plantadesp.Enabled	=	True
em_fecha.Enabled			=	True
em_numero.Enabled			=	True

pb_imprimir.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_grabar.Enabled			=	False
pb_insertar.Enabled		=	False

dw_destino.Enabled		=	True
sle_solic.Enabled			=	True
dw_destino.Object.dest_codigo[1] = li_null
sle_solic.Text							=	''
em_numero.Text							=	''
sle_solic.Enabled		   =	False

dw_cliente.SetFocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer taborder = 90
end type

event pb_lectura::clicked;call super::clicked;ddlb_tipocond.Enabled	=	False
dw_cliente.Enabled		=	False
dw_plantadesp.Enabled	=	False
em_fecha.Enabled			=	False
em_numero.Enabled			=	False

dw_1.SetFocus()
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer y = 788
integer taborder = 130
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer y = 608
integer taborder = 120
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer y = 1532
integer taborder = 160
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer y = 1148
integer taborder = 150
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 3186
integer y = 968
integer taborder = 140
end type

event pb_grabar::clicked;call super::clicked;Integer li_fila, li_pallet

li_fila=dw_1.rowcount()

if li_fila>0 then
	li_pallet = dw_1.getitemnumber(li_fila, "paen_numero")
end if
	
	
end event

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_inspecpaldet_cambio_mercado
integer y = 620
integer width = 3017
integer height = 1152
integer taborder = 110
string dataobject = "dw_mues_inspecpaldet_cambio_mercado"
end type

event dw_1::itemchanged;String	ls_columna
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
		ELSE
			dw_1.SetColumn(2)
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
		lstr_busq.argum[6]	=	''  //se agrega porque la ventana de busqueda se cae en filtro
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
					lstr_busq.argum[6]	=	'' 
					
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

type em_numero from editmask within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 517
integer y = 348
integer width = 526
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

event modified;Integer li_Null

SetNull(li_Null)

istr_mant.argumento[4]	=	This.Text

IF NoExisteInspeccion(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),dw_plantadesp.Object.plde_codigo[1],dw_cliente.Object.clie_codigo[1]) THEN
	dw_destino.Enabled	=	TRUE
	sle_solic.Enabled		=	TRUE
	dw_destino.Object.dest_codigo[1] = li_Null
	sle_solic.Text	=	''
	dw_destino.Setfocus()
END IF




end event

type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 517
integer y = 228
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

type st_4 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 169
integer y = 248
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

type dw_plantadesp from datawindow within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 2085
integer y = 228
integer width = 969
integer height = 92
integer taborder = 50
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

type st_2 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 1723
integer y = 248
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

type dw_cliente from datawindow within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 517
integer y = 108
integer width = 1170
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

type st_1 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 169
integer y = 124
integer width = 293
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

type st_3 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 169
integer y = 360
integer width = 251
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
string text = "Número"
boolean focusrectangle = false
end type

type dw_destino from datawindow within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 2085
integer y = 348
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

ls_columna	= dwo.Name

CHOOSE CASE ls_columna
	CASE "dest_codigo"
		istr_mant.argumento[5]	=	data
END CHOOSE
end event

type st_5 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 1723
integer y = 380
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
string text = "Destino"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 1723
integer y = 124
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
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 2085
integer y = 112
integer width = 526
integer height = 92
integer taborder = 20
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

type dw_2 from uo_dw within w_mant_mues_inspecpaldet_cambio_mercado
boolean visible = false
integer x = 165
integer y = 1424
integer width = 2885
integer height = 520
integer taborder = 40
boolean bringtotop = true
string dataobject = "dw_mues_inspecpalenc_cambio_mercado"
boolean hscrollbar = true
end type

type st_7 from statictext within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 169
integer y = 480
integer width = 325
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
string text = "Solicitante"
boolean focusrectangle = false
end type

type sle_solic from singlelineedit within w_mant_mues_inspecpaldet_cambio_mercado
integer x = 517
integer y = 464
integer width = 2441
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
integer limit = 60
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

