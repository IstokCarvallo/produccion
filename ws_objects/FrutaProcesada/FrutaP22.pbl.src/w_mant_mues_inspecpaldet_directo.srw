$PBExportHeader$w_mant_mues_inspecpaldet_directo.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_mues_inspecpaldet_directo from w_mant_directo
end type
type em_numero from editmask within w_mant_mues_inspecpaldet_directo
end type
type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpaldet_directo
end type
type st_4 from statictext within w_mant_mues_inspecpaldet_directo
end type
type dw_plantadesp from datawindow within w_mant_mues_inspecpaldet_directo
end type
type st_2 from statictext within w_mant_mues_inspecpaldet_directo
end type
type dw_cliente from datawindow within w_mant_mues_inspecpaldet_directo
end type
type st_1 from statictext within w_mant_mues_inspecpaldet_directo
end type
type st_3 from statictext within w_mant_mues_inspecpaldet_directo
end type
type dw_destino from datawindow within w_mant_mues_inspecpaldet_directo
end type
type st_5 from statictext within w_mant_mues_inspecpaldet_directo
end type
type st_6 from statictext within w_mant_mues_inspecpaldet_directo
end type
type em_fecha from editmask within w_mant_mues_inspecpaldet_directo
end type
type dw_2 from uo_dw within w_mant_mues_inspecpaldet_directo
end type
type st_7 from statictext within w_mant_mues_inspecpaldet_directo
end type
type sle_solic from singlelineedit within w_mant_mues_inspecpaldet_directo
end type
type cbx_fumigacion from checkbox within w_mant_mues_inspecpaldet_directo
end type
type st_8 from statictext within w_mant_mues_inspecpaldet_directo
end type
type sle_detalle from singlelineedit within w_mant_mues_inspecpaldet_directo
end type
end forward

global type w_mant_mues_inspecpaldet_directo from w_mant_directo
integer x = 155
integer y = 156
integer width = 3502
integer height = 1940
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
sle_solic sle_solic
cbx_fumigacion cbx_fumigacion
st_8 st_8
sle_detalle sle_detalle
end type
global w_mant_mues_inspecpaldet_directo w_mant_mues_inspecpaldet_directo

type variables
DataWindowChild	dwc_plantas, dwc_mercado, dwc_especie, dwc_variedad, &
						dwc_embalaje, dwc_tipopalemb

Integer 	ii_pallet, ii_nuevo,ii_tipo, il_fumiga
String	ls_detalledest, is_nombre
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean duplicado (string as_pallet)
public function boolean noexistepallet (string as_pallet)
public function boolean noexistepallet_ii ()
public function long buscainspeccion (integer planta)
public function boolean existedestino (integer ai_destino)
protected function boolean noexisteinspeccion (integer ai_tipo, long al_numero, integer ai_planta, integer ai_cliente)
public subroutine actualiza_inspecciones ()
end prototypes

event ue_validaborrar();Integer li_estado

IF dw_1.GetRow()>0 THEN
	li_estado	=	dw_1.GetItemNumber(dw_1.GetRow(),"paen_estado")
	
	IF li_estado=2 THEN
		MessageBox("Error", "Pallet fue Despachado, no se puede eliminar " + &
						"de esta inspección.~r~rIngrese o seleccione otro Pallet.")
						
		Message.DoubleParm	=	-1
	ELSEIF li_estado=3 THEN
//		MessageBox("Error", "Pallet fue Repalletizado, no se puede eliminar " + &
//						"de esta inspección.~r~rIngrese o seleccione otro Pallet.")
//						
//		Message.DoubleParm	=	-1
	END IF
END IF
end event

protected function boolean wf_actualiza_db ();Long numero, ll_fila, ll_numero
Integer li_planta, li_movto, li_tipoin, li_cliente


IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF dw_2.Update() = 1 AND dw_1.Update() = 1 THEN
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RETURN False		
	ELSE
		em_numero.text = istr_mant.argumento[4]
		RETURN True
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	RETURN False
END IF


//Numero = Long(istr_mant.argumento[4])
//li_planta = Integer(istr_mant.argumento[2])
//li_tipoin = Integer(istr_mant.argumento[3])
//
//IF li_tipoin = 1 THEN
//	li_movto = 5
//ELSE
//	li_movto = 6
//END IF	
//
///*actualiza numero actual en correlativos */
//update DBA.CORRELMOVIMIENTOS set
//COMO_ACTUAl = :numero
//where plde_codigo = :li_planta
//And	como_tipomv = :li_movto;

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
	INTO	:li_tipopa, 	  :ls_especie, 	 :ls_variedad,    :ls_embalaje, 
			:ll_cajas,  	  :li_inspec, 		 :li_estado, 		:li_fumiga
	FROM	dba.palletencab as pae, dba.especies as esp,
			dba.variedades as var, dba.embalajesprod as emb
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
ELSEIF li_inspec = 5	THEN
	MessageBox("Atención","Nro. de Pallet se Encuentra en Estado Pendiente.~r~r" + &
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
				pb_grabar.Enabled = True
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
				pb_grabar.Enabled = True
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
	pb_grabar.Enabled = True
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
			dba.variedades as var, dba.embalajesprod as emb
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

public function long buscainspeccion (integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

//li_planta	=	planta
//li_tipoins = Integer(istr_mant.argumento[3])
//
//IF li_tipoins = 1 THEN
//	li_movto = 5
//ELSE
//	li_movto = 6
//END IF	
//
//Select max(inpe_numero) 
//Into  :ll_numero
//From dba.inspecpalenc
//Where plde_codigo = :li_planta
//and inpe_tipoin = :li_tipoins;
//
//Select como_inicia, como_actual, como_termin
//Into	:ll_numero2, :ll_actual, :ll_fin
//from DBA.CORRELMOVIMIENTOS 
//Where plde_codigo = :li_planta
//and	COMO_TIPOMV = :li_movto;
//
//IF ll_actual >= ll_fin THEN
//	Return 0
//END IF	
//
//ll_fin = ll_fin - 3
//
//IF ll_actual >= ll_fin THEN 
//	MessageBox("Advertencia","Quedan menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
//END IF	
//
//IF sqlca.SQLCode = -1 THEN
//		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
//END IF
//
//IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
//	ll_numero = ll_numero2
//END IF	
//	
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura tabla inspecpalenc")
//ELSEIF sqlca.SQLCode = 0 THEN
//	ll_numero++
//END IF

RETURN ll_numero

end function

public function boolean existedestino (integer ai_destino);Integer	li_Cliente, li_Planta
Long		ll_Numero
String	ls

SELECT distinct dest_codigo, dest_fumbla, dest_nombre
INTO :ll_Numero, :il_fumiga, :is_nombre
FROM dba.destinos
WHERE dest_codigo	=	:ai_destino;

IF IsNull(ll_Numero) OR ll_Numero = 0 THEN
	MessageBox("Cuidado","Destino No Existe")
	RETURN True
END IF
RETURN False	
end function

protected function boolean noexisteinspeccion (integer ai_tipo, long al_numero, integer ai_planta, integer ai_cliente);String	ls_Solnom
Integer	li_Planta,li_Destino, li_Cliente, li_estado
Date     ld_fecha

  SELECT Distinct clie_codigo,plde_codigo,   
         dest_codigo,inpe_fechai,   
         inpe_solnom,inpe_estado  
    INTO :li_Cliente,:li_Planta,
	      :li_Destino,
			:ld_Fecha,:ls_Solnom,:li_estado  
    FROM dba.inspecpalenc  
   WHERE inpe_tipoin = :ai_tipo  AND  
         inpe_numero = :al_numero AND
			plde_codigo = :ai_planta AND
			clie_codigo	= :ai_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Inspecpalenc  ")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Inspección No Existe.", Exclamation!, Ok!)
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

public subroutine actualiza_inspecciones ();Integer	li_cont, li_planta, li_cliente, li_tipo
Long		ll_numero
String	ls_detalle

li_cliente 	= dw_cliente.Object.clie_codigo[1]
li_planta 	= dw_plantadesp.Object.plde_codigo[1]
ll_numero 	= Long(em_numero.Text)
li_tipo		= 1
ls_detalle  = sle_detalle.Text

SELECT count()
INTO :li_cont
FROM dba.inspecpalenc
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	inpe_numero = :ll_numero	
AND	inpe_tipoin = :li_tipo;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Inspecpalenc  ")
	RETURN 
ELSEIF li_cont > 1 THEN
	UPDATE dba.inspecpalenc SET
	inpe_desdet = :ls_detalle
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND	inpe_numero = :ll_numero	
	AND	inpe_tipoin = :li_tipo
	AND 	isnull(inpe_desdet,'') = '';
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Inspecpalenc  ")
		Commit;
		RETURN 
	END IF
	Commit;
END IF
		

end subroutine

event open;ii_tipo	=	Integer(Message.StringParm)
Long ll_fila
Integer	li_cliente, li_fumiga
String ls_null

x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 1993
im_menu	= m_principal

istr_mant.argumento[10] = '2'

IF ii_tipo = 2 THEN
	IF Integer(istr_mant.argumento[10]) <> 1 THEN
		This.ParentWindow().ToolBarVisible	=	True
		im_menu.Item[1].Item[6].Enabled		=	True
		im_menu.Item[7].Visible					=	False
		This.Icon									=	Gstr_apl.Icono
	END IF
ELSE
	im_menu	= m_principal
	im_menu.Item[1].Item[6].Enabled		=	True
	im_menu.Item[7].Visible					=	False
	This.Icon									=	Gstr_apl.Icono
	SetNull(ls_null)
	
	istr_mant	= Message.PowerObjectParm
		
	li_cliente	=	Integer(istr_mant.argumento[1])
	
	dw_cliente.SetTransObject(Sqlca)
	
	dw_destino.GetChild("dest_codigo",dwc_mercado)
	dwc_mercado.SetTransObject(Sqlca)
	dwc_mercado.Retrieve(0)
	dw_destino.SetTransObject(Sqlca)
	ll_fila = dw_destino.InsertRow(0)
	dw_destino.SetItem(ll_fila,"dest_codigo",Integer(istr_mant.argumento[5]))
	
	dw_1.GetChild("dest_codigo", dwc_mercado)
	dwc_mercado.SetTransObject(Sqlca)
	dwc_mercado.Retrieve(0)
		
	ll_fila = dw_cliente.InsertRow(0)
	dw_cliente.SetItem(ll_fila,"clie_codigo",li_cliente)
	
	dw_plantadesp.GetChild("plde_codigo",dwc_plantas)
	dwc_plantas.SetTransObject(Sqlca)
	dwc_plantas.Retrieve(1)			//Plantas de Despacho
	
	dw_plantadesp.SetTransObject(Sqlca)
	dw_plantadesp.InsertRow(0)
	dw_plantadesp.SetItem(1,"plde_codigo",Integer(istr_mant.argumento[2]))
	
	This.Icon =	Gstr_apl.Icono
	
	dw_1.SetTransObject(sqlca)
	dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
	dw_1.SetRowFocusIndicator(Hand!)
	dw_1.Modify("DataWindow.Footer.Height = 110")
	
	ddlb_tipocond.SelectItem(Integer(istr_mant.argumento[3]))
	em_numero.Text	= istr_mant.argumento[4]
	em_fecha.Text	= istr_mant.argumento[6]
	sle_solic.Text	= istr_mant.argumento[7]	
	
	li_fumiga = Integer(istr_mant.argumento[15])
	IF li_fumiga = 1 THEN
		cbx_fumigacion.enabled = True
		//cbx_fumigacion.Checked = True
	END IF	
	
	pb_insertar.Enabled = False
	pb_lectura.Enabled = False
END IF

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

dw_cliente.SetTransObject(Sqlca)

dw_destino.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(0)
dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)

dw_1.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(0)

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
istr_mant.Argumento[7]	=	""

TriggerEvent("ue_recuperadatos")
Close(w_mant_mues_inspecpalenc)



end event

on w_mant_mues_inspecpaldet_directo.create
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
this.cbx_fumigacion=create cbx_fumigacion
this.st_8=create st_8
this.sle_detalle=create sle_detalle
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
this.Control[iCurrent+16]=this.cbx_fumigacion
this.Control[iCurrent+17]=this.st_8
this.Control[iCurrent+18]=this.sle_detalle
end on

on w_mant_mues_inspecpaldet_directo.destroy
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
destroy(this.cbx_fumigacion)
destroy(this.st_8)
destroy(this.sle_detalle)
end on

event ue_antesguardar;Long	ll_fila = 1

//IF em_numero.Text = '' OR Isnull(em_numero.Text) THEN
//	istr_mant.argumento[4] = //String(buscainspeccion(Integer(istr_mant.argumento[2])))
//ELSE
//	istr_mant.argumento[4] = em_numero.Text
//END IF

istr_mant.argumento[4] = em_numero.Text

IF sle_detalle.Text <> '' THEN
	ls_detalledest = sle_detalle.Text
ELSE
	ls_detalledest 	= 	is_nombre	
	sle_detalle.Text 	=	is_nombre	
END IF	

IF Long(Istr_mant.argumento[4]) = 0 OR Isnull(Istr_mant.argumento[4]) THEN
	MessageBox("Advertencia","Falta Ingresar Número de Inspección.")
	Message.DoubleParm = -1
	Return
END IF	

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! OR isnull(dw_1.Object.paen_numero[ll_fila]) THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
			dw_1.Object.inpe_tipoin[ll_fila]	=	Integer(istr_mant.argumento[3])
			dw_1.Object.inpe_numero[ll_fila]	=	Long(istr_mant.argumento[4])
			dw_1.Object.clie_codigo[ll_fila]	=	Integer(istr_mant.argumento[1])
			dw_1.Object.plde_codigo[ll_fila]	=	Integer(istr_mant.argumento[2])
			dw_1.Object.inpe_secuen[ll_fila]	=	1
			dw_1.Object.inpd_fechai[ll_fila]	=	Date(istr_mant.argumento[6])
			dw_1.Object.dest_codigo[ll_fila]	=	Integer(istr_mant.argumento[5])
						
		END IF
		ll_fila ++
	END IF
LOOP 

IF ii_nuevo	=	0 THEN
	istr_mant.argumento[7]		=	sle_solic.Text
	dw_2.Object.inpe_tipoin[1]	=	Integer(istr_mant.argumento[3])
	dw_2.Object.inpe_numero[1]	=	Long(istr_mant.argumento[4])
	dw_2.Object.clie_codigo[1]	=	Integer(istr_mant.argumento[1])
	dw_2.Object.plde_codigo[1]	=	Integer(istr_mant.argumento[2])
	dw_2.Object.inpe_secuen[1]	=	1
	dw_2.Object.dest_codigo[1]	=	Integer(istr_mant.argumento[5])
	dw_2.Object.inpe_fechai[1]	=	Date(istr_mant.argumento[6])
	dw_2.Object.inpe_solnom[1]	=	istr_mant.argumento[7]
	dw_2.Object.inpe_estado[1]	=	5
	dw_2.Object.inpe_desdet[1]	=	ls_detalledest
		
ELSE	
	istr_mant.argumento[7]		=	sle_solic.Text
	dw_2.Object.inpe_solnom[1]	=	istr_mant.argumento[7]
	dw_2.Object.inpe_desdet[1]	=	ls_detalledest
END IF

end event

event ue_recuperadatos;Long	ll_fila, ll_fila1, respuesta, ll_numero
Integer	li_destino, li_estado, li_planta, li_cliente, li_condicion

ii_nuevo		=	0
ll_numero 	= Long(istr_mant.argumento[4])
li_cliente 	= Integer(istr_mant.argumento[1])
li_planta 	= Integer(istr_mant.argumento[2])
li_condicion = Integer(istr_mant.argumento[3])

 SELECT Distinct inpe_estado  
    INTO :li_estado  
    FROM dba.inspecpalenc  
   WHERE inpe_tipoin = :li_condicion  AND  
         inpe_numero = :ll_numero AND
			plde_codigo = :li_planta AND
			clie_codigo	= :li_cliente;
			
IF isnull(li_estado) OR li_estado = 0 OR li_estado = 5 THEN
	DO
		ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
									Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
									Integer(istr_mant.argumento[5]))
		IF ll_fila = -1 THEN
			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
		ELSE
			
			IF ll_fila = 0 THEN
				 st_5.Visible			=	True
				 dw_destino.Enabled	=	True
				 dw_destino.Visible	=	True
				 //ddlb_tipocond.Enabled	=	True
				 //em_numero.Enabled	=	True
			END IF
			ll_fila1	=	dw_2.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
											 Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
			IF ll_fila1 > 0 THEN 
				ii_nuevo	=	1
				dw_1.SetRow(1)
				dw_1.SetFocus()
				pb_eliminar.Enabled	=	True
				pb_insertar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_lectura.Enabled = True
				
				li_destino = dw_2.Object.dest_codigo[1]
				
				existedestino(Integer(li_destino)) 
				IF il_fumiga = 1 THEN
					cbx_fumigacion.Enabled = True
				ELSE	
					cbx_fumigacion.Enabled = False
				END IF	
				
				pb_insertar.SetFocus()
				dw_destino.Object.dest_codigo[1] = dw_2.Object.dest_codigo[1]
				istr_mant.Argumento[5] = String(dw_2.Object.dest_codigo[1])
				sle_solic.Text = dw_2.Object.inpe_solnom[1]
				dw_destino.Enabled = False
				sle_detalle.Text = dw_2.Object.inpe_desdet[1]
			END IF
		END IF
	LOOP WHILE respuesta = 1
	
	IF respuesta = 2 THEN Close(This)
ELSE
	MessageBox("Advertencia","NO es Posible Modificar Inspección Aprobada'")
	ddlb_tipocond.Enabled	=	True
	dw_cliente.Enabled		=	True
	dw_plantadesp.Enabled	=	True
	em_fecha.Enabled			=	True
	//dw_destino.Enabled		=  False
	em_numero.Text				=	''
	em_numero.Enabled			=	True
	ddlb_tipocond.Enabled	=	True
	//sle_solic.Enabled			=  False
	em_numero.SetFocus()
	Return
END IF	
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
											Integer(istr_mant.argumento[2]),0)

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

event close;Boolean	Valida
Window	ventana
Integer	li_vta

//ventana	= This.ParentWindow().GetFirstSheet()
//
//IF IsValid(ventana) THEN
//	li_vta++
//
//	DO
//		ventana	= this.ParentWindow().GetNextSheet(ventana)
//		valida	= IsValid(ventana)
//		IF valida THEN li_vta++
//	LOOP WHILE valida
//END IF
//
//IF li_vta = 1 THEN
//	This.ParentWindow().ToolBarVisible	= False
//	im_menu.Item[1].Item[6].Enabled		= False
//	im_menu.Item[7].Visible					= False
//END IF

GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event ue_guardar;SetPointer(HourGlass!)
Integer	li_planta, li_cliente
Long	ll_fila, ll_numero

ib_ok = True

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

IF dw_1.AcceptText() = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
	
	actualiza_inspecciones()
	
	IF cbx_fumigacion.Checked = True THEN
		li_planta = dw_plantadesp.Object.plde_codigo[1]
		li_cliente = dw_cliente.Object.clie_codigo[1]
		
		FOR ll_fila = 1 TO dw_1.RowCount()
			ll_numero = dw_1.Object.paen_numero[ll_fila]
			
			update dba.palletencab set
				cond_codigo = 1 where 
				clie_codigo = :li_cliente and
				plde_codigo = :li_planta and
				paen_numero = :ll_numero
				Using Sqlca;
				
			update dba.palletfruta set
				cond_codigo = 1 where 
				clie_codigo = :li_cliente and
				plde_codigo = :li_planta and
				paen_numero = :ll_numero
				Using Sqlca;
		NEXT		 
	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN


//IF  dw_1.Object.inpe_secuen[il_fila] <> 90 THEN
	IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
		IF dw_1.DeleteRow(0) = 1 THEN
			ib_borrar = False
			pb_eliminar.Enabled = True
			pb_grabar.Enabled = True
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
	 IF dw_1.RowCount() = 0 THEN
	//		pb_eliminar.Enabled = False
	//		pb_grabar.Enabled = False
		ELSE
			il_fila = dw_1.GetRow()
		END IF
	END IF
//ELSE
//	MessageBox("Atención", "Pallet Producto de Repalletizado, ~r~r(No es Considerado en Histórico)")
//END IF	
end event

event ue_nuevo;IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

il_fila = dw_1.InsertRow(0)

IF Isnull(dw_destino.Object.dest_codigo[1]) OR dw_destino.Object.dest_codigo[1] = 0 THEN
	dw_1.Object.dest_codigo.Protect	=	0
ELSE	
	dw_1.Object.dest_codigo.Protect	=	1
	dw_1.Object.dest_codigo[il_fila] = dw_destino.Object.dest_codigo[1]
END IF

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn(1)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_inspecpaldet_directo
integer x = 82
integer width = 3017
integer height = 604
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_inspecpaldet_directo
integer x = 3186
integer y = 444
integer taborder = 110
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
dw_destino.Enabled 		=  True 
pb_imprimir.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_grabar.Enabled			=	False
pb_insertar.Enabled		=	False
pb_lectura.Enabled		=	False
sle_solic.Enabled			=  True
istr_mant.Argumento[5] = ''
istr_mant.Argumento[4] = ''

//dw_destino.Enabled		=	False
sle_solic.Enabled			=	True
dw_destino.Object.dest_codigo[1] = li_null
sle_solic.Text							=	''
em_numero.Text							=	''
sle_detalle.Text						=	''
//sle_solic.Enabled		   =	False

dw_cliente.SetFocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_inspecpaldet_directo
integer x = 3186
integer y = 132
integer taborder = 100
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;pb_insertar.Enabled = True

ddlb_tipocond.Enabled	=	False
dw_cliente.Enabled		=	False
dw_plantadesp.Enabled	=	False
em_fecha.Enabled			=	False
//dw_destino.Enabled		=  False
em_numero.Enabled			=	False
ddlb_tipocond.Enabled	=	False
//sle_solic.Enabled			=  False
dw_1.SetFocus()
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_inspecpaldet_directo
integer x = 3186
integer y = 804
integer taborder = 140
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_inspecpaldet_directo
integer x = 3186
integer y = 620
integer taborder = 130
end type

event pb_insertar::clicked;IF Isnull(dw_destino.Object.dest_codigo[1]) OR dw_destino.Object.dest_codigo[1] = 0 THEN
	MessageBox("Atencion","Falta Ingreso del Destino.  ")
	dw_destino.SetFocus()
	Return 1
ELSE
	dw_destino.Enabled = False
END IF	

Parent.TriggerEvent("ue_nuevo")
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_inspecpaldet_directo
integer x = 3186
integer y = 1584
integer taborder = 170
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_inspecpaldet_directo
boolean visible = false
integer x = 3186
integer y = 1164
integer taborder = 160
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_inspecpaldet_directo
integer x = 3186
integer y = 984
integer taborder = 150
end type

event pb_grabar::clicked;Integer li_fila, li_pallet

Parent.TriggerEvent("ue_guardar")

li_fila=dw_1.rowcount()

if li_fila>0 then
	li_pallet = dw_1.getItemNumber(li_fila, "paen_numero")
ELSE
	pb_grabar.Enabled = False
end if
	
	
end event

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_inspecpaldet_directo
integer y = 688
integer width = 3017
integer height = 1072
integer taborder = 120
string dataobject = "dw_mues_inspecpaldet"
boolean hscrollbar = true
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
			pb_grabar.Enabled = False
			pb_eliminar.Enabled = False
			RETURN 1	
		ELSEIF NoExistePallet(data) THEN			
			This.SetItem(row,ls_columna,ll_null)			
   		RETURN 1
		ELSE
			dw_1.SetColumn(2)
		END IF
 END CHOOSE

end event

event dw_1::buttonclicked;str_busqueda	lstr_busq
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
		
//	CASE KeyTab!
//		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
//			Message.DoubleParm = 0
//			
//			Parent.TriggerEvent("ue_validaregistro")
//			
//			IF Message.DoubleParm = -1 THEN
//				This.SetRedraw(True)
//				RETURN -1
//			ELSE
//				Parent.TriggerEvent("ue_nuevo")
//				
//				This.SetFocus()
//				This.SetRedraw(True)
//				RETURN -1
//			END IF
//		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type em_numero from editmask within w_mant_mues_inspecpaldet_directo
integer x = 562
integer y = 320
integer width = 526
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Integer li_Null

SetNull(li_Null)

istr_mant.argumento[4]=This.text

//IF NoExisteInspeccion(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),dw_plantadesp.Object.plde_codigo[1],dw_cliente.Object.clie_codigo[1]) THEN
	//em_numero.Text = ''
	istr_mant.argumento[4] = em_numero.Text//String(buscainspeccion(Integer(istr_mant.argumento[2])))
	
	dw_destino.Enabled	=	TRUE
	sle_solic.Enabled		=	TRUE
//	TriggerEvent("ue_recuperadatos")
//	Parent.PostEvent("ue_recuperadatos")
	dw_destino.Object.dest_codigo[1] = li_Null
	sle_solic.Text							=	''
	dw_destino.Setfocus()
//ELSE	
	
	istr_mant.argumento[4] = em_numero.Text
	pb_lectura.Enabled = True
	
//END IF






	


end event

type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpaldet_directo
integer x = 562
integer y = 200
integer width = 526
integer height = 292
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
boolean sorted = false
string item[] = {"Inspección",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
end event

type st_4 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 105
integer y = 220
integer width = 334
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_mant_mues_inspecpaldet_directo
integer x = 2062
integer y = 200
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

type st_2 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 1778
integer y = 220
integer width = 279
integer height = 64
boolean bringtotop = true
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

type dw_cliente from datawindow within w_mant_mues_inspecpaldet_directo
integer x = 562
integer y = 88
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

type st_1 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 105
integer y = 104
integer width = 293
integer height = 64
boolean bringtotop = true
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

type st_3 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 105
integer y = 336
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Número"
boolean focusrectangle = false
end type

type dw_destino from datawindow within w_mant_mues_inspecpaldet_directo
integer x = 2062
integer y = 320
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
		IF existedestino(Integer(data)) THEN
			This.SetItem(1, "dest_codigo", li_null)
			RETURN 1
		ELSE
			istr_mant.argumento[5]	=	data
			IF il_fumiga = 1 THEN
				cbx_fumigacion.Enabled = True
			ELSE
				cbx_fumigacion.Enabled = False
			END IF
			pb_lectura.Enabled = True
		END IF	
		
END CHOOSE

IF istr_mant.argumento[5] <> '' OR ISnull(istr_mant.argumento[5]) =False THEN
	pb_lectura.Enabled = True
END IF	
end event

type st_5 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 1778
integer y = 336
integer width = 265
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 1778
integer y = 104
integer width = 265
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_inspecpaldet_directo
integer x = 2062
integer y = 92
integer width = 526
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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

type dw_2 from uo_dw within w_mant_mues_inspecpaldet_directo
boolean visible = false
integer x = 128
integer y = 1104
integer width = 2885
integer height = 520
integer taborder = 40
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_inspecpalenc"
boolean hscrollbar = true
end type

type st_7 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 105
integer y = 444
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Solicitante"
boolean focusrectangle = false
end type

type sle_solic from singlelineedit within w_mant_mues_inspecpaldet_directo
integer x = 562
integer y = 428
integer width = 2441
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 60
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

type cbx_fumigacion from checkbox within w_mant_mues_inspecpaldet_directo
integer x = 2624
integer y = 100
integer width = 448
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
boolean enabled = false
string text = "Fumigación"
end type

type st_8 from statictext within w_mant_mues_inspecpaldet_directo
integer x = 105
integer y = 548
integer width = 453
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
string text = "Detalle Destino"
boolean focusrectangle = false
end type

type sle_detalle from singlelineedit within w_mant_mues_inspecpaldet_directo
integer x = 562
integer y = 540
integer width = 2441
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 400
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

