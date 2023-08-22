$PBExportHeader$w_maed_planillainspecdestino.srw
$PBExportComments$Encabezado de Ingreso de Inspección en Destino USA
forward
global type w_maed_planillainspecdestino from w_mant_encab_deta_csd
end type
type pb_copiar from picturebutton within w_maed_planillainspecdestino
end type
end forward

global type w_maed_planillainspecdestino from w_mant_encab_deta_csd
integer width = 5056
integer height = 2356
string title = "PLANILLA  DE INSPECCION DE UVA DE MESA EN DESTINO"
string menuname = ""
boolean clientedge = true
boolean ib_deshace = false
event ue_copiadatos ( )
pb_copiar pb_copiar
end type
global w_maed_planillainspecdestino w_maed_planillainspecdestino

type variables
Integer ii_opcion
DataWindowChild 							idwc_clientes,idwc_nave,idwc_recibidor,idwc_puerto,idwc_inspector,idwc_especie,&
												idwc_productor,idwc_calibre,idwc_variedad,idwc_embalaje,idwc_etiqueta,idwc_destinos,&
												idwc_mercado, idwc_sitio 

w_mant_deta_planillainspdestinodet	iw_mantencion

uo_especie              				iuo_especie      
uo_naves                				iuo_naves
uo_recibimercado        				iuo_recibimercado
uo_ctlcalinspectores    				iuo_ctlcalinspectores
uo_puertos              				iuo_puertos
uo_variedades           				iuo_variedades
uo_productores          				iuo_productores
uo_calibre              				iuo_calibre
uo_embalajesprod        				iuo_embalajes
uo_etiquetas            				iuo_etiquetas
uo_destinos             				iuo_destinos
uo_mercado              				iuo_mercado
uo_sitioinspeccion      				iuo_sitio






end variables

forward prototypes
protected function integer wf_modifica ()
public subroutine existelote ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine soloconsulta ()
public function boolean trae_planilla (long al_planilla, integer ai_cliente, integer ai_especie, integer ai_mercado, integer ai_destino)
public subroutine copiaencabezado ()
end prototypes

event ue_copiadatos();dw_2.SetItem(1,"clie_codigo",Integer(istr_mant.argumento[6])) //cliente
dw_2.SetItem(1,"espe_codigo",Integer(istr_mant.argumento[8])) //especie
dw_2.SetItem(1,"merc_codigo",Integer(istr_mant.argumento[15]))//mercado
dw_2.SetItem(1,"dest_codigo",Integer(istr_mant.argumento[16]))//destino
dw_2.SetItem(1,"nave_tipotr",istr_mant.argumento[18])			  //tipo transporte
dw_2.SetItem(1,"nave_codigo",Integer(istr_mant.argumento[2])) //nave
dw_2.SetItem(1,"cpde_bode01",istr_mant.argumento[19])			  //bodega 1
dw_2.SetItem(1,"cpde_bode02",istr_mant.argumento[20])			  //bodega 2
dw_2.SetItem(1,"cpde_bode03",istr_mant.argumento[21])			  //bodega 3
dw_2.SetItem(1,"cpde_bode04",istr_mant.argumento[22])			  //bodega 4
dw_2.SetItem(1,"cpde_bode05",istr_mant.argumento[23])			  //bodega 5
dw_2.SetItem(1,"cpde_conten",istr_mant.argumento[24])         //contenedor
dw_2.SetItem(1,"puer_codigo",Integer(istr_mant.argumento[5])) //puerto
dw_2.SetItem(1,"ccsi_codigo",Integer(istr_mant.argumento[25]))//lugar de inspección
dw_2.SetItem(1,"cpde_fecarr",Date(istr_mant.argumento[26]))   //fecha arribo
dw_2.SetItem(1,"cpde_fecdes",Date(istr_mant.argumento[27]))   //fecha descarga
dw_2.SetItem(1,"cpde_fecins",Date(istr_mant.argumento[28]))   //fecha inspección
dw_2.SetItem(1,"cpde_fecfum",Date(istr_mant.argumento[29]))   //fecha fumigación
dw_2.SetItem(1,"reci_codigo",Integer(istr_mant.argumento[3])) //recibidor
dw_2.SetItem(1,"ccin_codigo",Integer(istr_mant.argumento[4])) //inspector
dw_2.SetItem(1,"cpde_reclam",Integer(istr_mant.argumento[30]))//reclamo

dw_2.SetItem(1,"prod_codigo",Long(istr_mant.argumento[31]))//productor
dw_2.SetItem(1,"vari_codigo",Integer(istr_mant.argumento[32]))//variedad
dw_2.SetItem(1,"cpde_tamlot",Long(istr_mant.argumento[33]))//tamaño lote
dw_2.SetItem(1,"emba_codigo",istr_mant.argumento[34])//embalaje
dw_2.SetItem(1,"etiq_codigo",Integer(istr_mant.argumento[35]))//etiqueta
dw_2.SetItem(1,"cpde_calibr",istr_mant.argumento[36])//calibre




end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0


RETURN 1
end function

public subroutine existelote ();
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.protect = 0             
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
   dw_2.Object.cpde_numero.protect = 0            
   dw_2.Object.cpde_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.prod_codigo.protect = 0             
	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.protect = 0             
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cpde_tamlot.protect = 0             
	dw_2.Object.cpde_tamlot.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.emba_codigo.protect = 0             
	dw_2.Object.emba_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.etiq_codigo.protect = 0             
	dw_2.Object.etiq_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cpde_calibr.protect = 0             
	dw_2.Object.cpde_calibr.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cpde_reclam.protect = 0             
	dw_2.Object.cpde_reclam.BackGround.Color	=	RGB(255,255,255)
ELSE
   dw_2.Object.cpde_numero.protect = 1            
   dw_2.Object.cpde_numero.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.clie_codigo.protect = 1             
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.prod_codigo.protect = 1             
	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.vari_codigo.protect = 1             
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cpde_tamlot.protect = 1             
	dw_2.Object.cpde_tamlot.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.emba_codigo.protect = 1             
	dw_2.Object.emba_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.etiq_codigo.protect = 1             
	dw_2.Object.etiq_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cpde_calibr.protect = 1             
	dw_2.Object.cpde_calibr.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cpde_reclam.protect = 1             
	dw_2.Object.cpde_reclam.BackGround.Color	=	RGB(192,192,192)
END IF

end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()
	
IF ls_Columna <> "clie_codigo" AND &
	(dw_2.Object.clie_codigo[1]) = 0 OR IsNull(dw_2.Object.clie_codigo[1]) THEN
	lb_Estado	=	False
END IF

//IF ls_Columna <> "cpde_numero" AND &
//	(dw_2.Object.cpde_numero[1]) = 0 OR IsNull(dw_2.Object.cpde_numero[1]) THEN
//	lb_Estado	=	False
//END IF


IF ls_Columna <> "nave_tipotr" AND &
	(dw_2.Object.nave_tipotr[1]) = '' OR IsNull(dw_2.Object.nave_tipotr[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "nave_codigo" AND &
	(dw_2.Object.nave_codigo[1]) = 0 OR IsNull(dw_2.Object.nave_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "cpde_fecarr" AND &
	IsNull(dw_2.Object.cpde_fecarr[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "reci_codigo" AND &
	(dw_2.Object.reci_codigo[1]) = 0 OR IsNull(dw_2.Object.reci_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "puer_codigo" AND &
	(dw_2.Object.puer_codigo[1]) = 0 OR IsNull(dw_2.Object.puer_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "dest_codigo" AND &
	(dw_2.Object.dest_codigo[1]) = 0 OR IsNull(dw_2.Object.dest_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "prod_codigo" AND &
	(dw_2.Object.prod_codigo[1]) = 0 OR IsNull(dw_2.Object.prod_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "vari_codigo" AND &
	(dw_2.Object.vari_codigo[1]) = 0 OR IsNull(dw_2.Object.vari_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "emba_codigo" AND &
	(dw_2.Object.emba_codigo[1]) = '' OR IsNull(dw_2.Object.emba_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "etiq_codigo" AND &
	(dw_2.Object.etiq_codigo[1]) = 0 OR IsNull(dw_2.Object.etiq_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "cpde_calibr" AND &
	(dw_2.Object.cpde_calibr[1]) = '' OR IsNull(dw_2.Object.cpde_calibr[1]) THEN
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_Estado
pb_grabar.Enabled		=	lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True,False) =	1	THEN
			
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					
				END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
  
	
		IF dw_2.Update(True,False) =	1	THEN
			IF dw_1.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
		
	
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
RETURN true
end function

public subroutine soloconsulta ();dw_2.Object.cpde_numero.Protect  =  1
dw_2.Object.nave_tipotr.Protect  =  1
dw_2.Object.nave_codigo.Protect  =  1
dw_2.Object.puer_codigo.Protect  =  1
dw_2.Object.dest_codigo.Protect  =  1
dw_2.Object.ccsi_codigo.Protect  =  1
dw_2.Object.clie_codigo.Protect  =  1
dw_2.Object.cpde_bode01.Protect  =  1
dw_2.Object.cpde_bode02.Protect  =  1
dw_2.Object.cpde_bode03.Protect  =  1
dw_2.Object.cpde_bode04.Protect  =  1
dw_2.Object.cpde_bode05.Protect  =  1
dw_2.Object.cpde_conten.Protect  =  1
dw_2.Object.merc_codigo.Protect  =  1
dw_2.Object.reci_codigo.Protect  =  1
dw_2.Object.ccin_codigo.Protect  =  1
dw_2.Object.cpde_fecarr.Protect  =  1
dw_2.Object.cpde_fecdes.Protect  =  1
dw_2.Object.cpde_fecfum.Protect  =  1
dw_2.Object.cpde_fecins.Protect  =  1
dw_2.Object.cpde_tamlot.Protect  =  1
dw_2.Object.prod_codigo.Protect  =  1
dw_2.Object.cpde_calibr.Protect  =  1
dw_2.Object.vari_codigo.Protect  =  1
dw_2.Object.emba_codigo.Protect  =  1
dw_2.Object.etiq_codigo.Protect  =  1

pb_buscar.Visible    = False
pb_nuevo.Visible     = False
pb_eliminar.Visible  = False
pb_grabar.Visible    = False
pb_ins_det.Visible   = False
pb_eli_det.Visible   = False


end subroutine

public function boolean trae_planilla (long al_planilla, integer ai_cliente, integer ai_especie, integer ai_mercado, integer ai_destino);Long 		ll_planilla

SELECT cpde_numero
  INTO :ll_planilla
  FROM dba.ctlcalplandestinosenc
 WHERE cpde_numero =	:al_planilla
   AND clie_codigo = :ai_cliente
	AND espe_codigo = :ai_especie
	AND merc_codigo = :ai_mercado
	AND dest_codigo = :ai_destino; 

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLANDESTINOSENC")
	RETURN FALSE
ELSEIF sqlca.sqlcode	=	100 THEN
	MessageBox("Atención","Planilla No existe por favor digite otra")
	RETURN FALSE
ELSE/*IF sqlca.sqlcode	=	0 THEN*/
	istr_mant.Argumento[6]  = String(ai_cliente)
	istr_mant.Argumento[1]  = String(al_planilla)
	istr_mant.Argumento[8]  = String(ai_especie)
	istr_mant.Argumento[15] = String(ai_mercado)
	istr_mant.Argumento[16] = String(ai_destino)
	This.TriggerEvent("ue_recuperadatos")
		
	
   pb_ins_det.Enabled	=	TRUE
   pb_eli_det.Enabled	=	TRUE
	pb_grabar.Enabled    =  TRUE
	pb_eliminar.Enabled  =  TRUE
	pb_imprimir.Enabled  =  TRUE
	pb_copiar.Enabled    =  FALSE
	
	RETURN TRUE
END IF
end function

public subroutine copiaencabezado ();Integer  li_secuencia, li_cliente, li_especie, li_LugIns, li_etiqueta, li_mercado,&
         li_nave, li_destino, li_Puerto, li_Inspector
Long		ll_Fila, ll_Recibidor , ll_lote, ll_productor
String   ls_Contenedor, ls_bode01, ls_bode02, ls_bode03, ls_bode04, ls_bode05, ls_tipotra
Date     ld_fechain, ld_Fecarr

li_cliente		=	dw_2.Object.clie_codigo[1]
li_especie     =  dw_2.Object.espe_codigo[1]
ll_Recibidor	=  dw_2.Object.reci_codigo[1]
li_Puerto	   =  dw_2.Object.puer_codigo[1]
li_LugIns     	=  dw_2.Object.ccsi_codigo[1]
li_Inspector   =  dw_2.Object.ccin_codigo[1]
ls_Contenedor  =  dw_2.Object.cpde_conten[1]
li_etiqueta    =  dw_2.Object.etiq_codigo[1]
li_mercado     =  dw_2.Object.merc_codigo[1]
ld_fechain     =  dw_2.Object.cpde_fecins[1]
ld_Fecarr		=  dw_2.Object.cpde_fecarr[1]
ls_tipotra     =  dw_2.Object.nave_tipotr[1]
li_nave        =  dw_2.Object.nave_codigo[1]
li_destino     =  dw_2.Object.dest_codigo[1]
ls_bode01		=	dw_2.Object.cpde_bode01[1]
ls_bode02		=	dw_2.Object.cpde_bode02[1]
ls_bode03		=	dw_2.Object.cpde_bode03[1]
ls_bode04		=	dw_2.Object.cpde_bode04[1]
ls_bode05		=	dw_2.Object.cpde_bode05[1]

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)
idwc_recibidor.Retrieve(li_mercado)
idwc_destinos.Retrieve(li_mercado)

dw_2.Object.clie_codigo[1] =	li_cliente
dw_2.Object.espe_codigo[1]	=	li_especie
dw_2.Object.reci_codigo[1]	=	ll_Recibidor	
dw_2.Object.puer_codigo[1]	=	li_Puerto
dw_2.Object.ccsi_codigo[1]	=	li_LugIns
dw_2.Object.ccin_codigo[1]	=	li_Inspector
dw_2.Object.cpde_conten[1]	=	ls_Contenedor
dw_2.Object.etiq_codigo[1] =	li_etiqueta
dw_2.Object.merc_codigo[1]	=  li_mercado     
dw_2.Object.cpde_fecins[1]	=	ld_fechain
dw_2.Object.cpde_fecarr[1]	=	ld_Fecarr
dw_2.Object.nave_tipotr[1]	=	ls_tipotra
dw_2.Object.nave_codigo[1]	=	li_nave
dw_2.Object.dest_codigo[1]	=	li_destino
dw_2.Object.cpde_bode01[1]	=	ls_bode01		
dw_2.Object.cpde_bode02[1]	=	ls_bode02		
dw_2.Object.cpde_bode03[1]	=	ls_bode03		
dw_2.Object.cpde_bode04[1]	=	ls_bode04		
dw_2.Object.cpde_bode05[1]	=	ls_bode05
end subroutine

on w_maed_planillainspecdestino.create
int iCurrent
call super::create
this.pb_copiar=create pb_copiar
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_copiar
end on

on w_maed_planillainspecdestino.destroy
call super::destroy
destroy(this.pb_copiar)
end on

event ue_seleccion;call super::ue_seleccion;String	ls_nula

SetNull(ls_nula)

istr_busq.argum[1]	=	String(gi_CodExport)

OpenWithParm(w_busc_ctlcalplanilladestino, istr_busq)
istr_busq = Message.PowerObjectParm

	IF istr_busq.argum[1] <> '' THEN
		istr_mant.argumento[6] 	= istr_busq.argum[1] //cliente
		istr_mant.argumento[1] 	= istr_busq.argum[2] //numero planilla
		istr_mant.argumento[8]  = istr_busq.argum[3] //especie
		istr_mant.argumento[15] = istr_busq.argum[4] //mercado
		istr_mant.argumento[16] = istr_busq.argum[5] //destino
		
		istr_mant.argumento[18] = istr_busq.argum[6] //tipo transporte
		istr_mant.argumento[2]  = istr_busq.argum[7] //nave
		istr_mant.argumento[19] = istr_busq.argum[8] //bodega 1
		istr_mant.argumento[20] = istr_busq.argum[9] //bodega 2
		istr_mant.argumento[21] = istr_busq.argum[10]//bodega 3
		istr_mant.argumento[22] = istr_busq.argum[11]//bodega 4
		istr_mant.argumento[23] = istr_busq.argum[12]//bodega 5
		istr_mant.argumento[24] = istr_busq.argum[13]//contenedor
		istr_mant.argumento[5]  = istr_busq.argum[14]//puerto
		istr_mant.argumento[25] = istr_busq.argum[15]//lugar de inspección
		istr_mant.argumento[26] = istr_busq.argum[16]//fecha arribo
		istr_mant.argumento[27] = istr_busq.argum[17]//fecha descarga
		istr_mant.argumento[28] = istr_busq.argum[18]//fecha inspección
		istr_mant.argumento[29] = istr_busq.argum[19]//fecha fumigación
		istr_mant.argumento[3]  = istr_busq.argum[20]//recibidor
		istr_mant.argumento[4]  = istr_busq.argum[21]//inspector
		istr_mant.argumento[30] = istr_busq.argum[22]//reclamo
		
		istr_mant.argumento[31] = istr_busq.argum[23]//productor
		istr_mant.argumento[32] = istr_busq.argum[24]//variedad
		istr_mant.argumento[33] = istr_busq.argum[25]//tamaño lote
		istr_mant.argumento[34] = istr_busq.argum[26]//embalaje
		istr_mant.argumento[35] = istr_busq.argum[27]//etiqueta
		istr_mant.argumento[36] = istr_busq.argum[28]//calibre
		
		
		IF istr_mant.Argumento[17] = '0' THEN		
			This.TriggerEvent("ue_recuperadatos")
		ELSE
			This.TriggerEvent("ue_copiadatos")
			pb_ins_det.Enabled = True
		END IF
			
	ELSE
		pb_buscar.SetFocus()
	END IF
	


	
	
end event

event ue_recuperadatos;long ll_fila_e, ll_fila_d, ll_fila_f, respuesta, ll_nave


DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_fila_e	= dw_2.Retrieve(Integer(Istr_mant.argumento[6]),Integer(Istr_mant.argumento[1]),&
	                            Integer(Istr_mant.argumento[15]),Integer(Istr_mant.argumento[8]),&
										 Integer(Istr_mant.argumento[16]))

	habilitaencab(False)							
  /*se envian argumentos al detalle para validar pallet*/
   istr_mant.argumento[11] = String(dw_2.Object.prod_codigo[1])
   istr_mant.argumento[10] = String(dw_2.Object.vari_codigo[1])
   istr_mant.argumento[14] = dw_2.Object.emba_codigo[1]
   istr_mant.argumento[13] = String(dw_2.Object.etiq_codigo[1])
   istr_mant.argumento[12] = dw_2.Object.cpde_calibr[1]
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		ll_nave							=	dw_2.Object.nave_codigo[1]
		
		dw_2.GetChild("nave_codigo", idwc_nave)
		idwc_nave.SetTransObject(sqlca)
		idwc_nave.Retrieve(dw_2.Object.nave_tipotr[1])
		
		dw_2.Object.nave_codigo[1] = ll_nave
		
		dw_2.SetItemStatus(1, 0, Primary!, NotModified!)
		
		DO
		
			ll_fila_d	= dw_1.Retrieve(Integer(Istr_mant.argumento[6]),Integer(Istr_mant.argumento[1]),&
			                            Integer(Istr_mant.argumento[15]),Integer(Istr_mant.argumento[8]),&
												 Integer(Istr_mant.argumento[16]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
												
			ELSE
								
			   pb_eliminar.Enabled	= TRUE
				pb_grabar.Enabled		= TRUE
				pb_imprimir.Enabled	= TRUE
				pb_ins_det.Enabled	= TRUE
				
				IF ll_fila_d > 0 THEN
				   pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
			
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
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)
idwc_recibidor.Retrieve(1)
idwc_destinos.Retrieve(1)
dw_2.Setitem(1,"clie_codigo",Gi_CodExport)
dw_2.Setitem(1,"espe_codigo",11)
dw_2.SetItem(1,"merc_codigo",1)
dw_2.SetItem(1,"dest_codigo",510)
dw_2.SetItem(1,"cpde_reclam",0)
dw_2.SetItem(1,"nave_tipotr","M")
dw_2.SetFocus()
dw_2.SetColumn("cpde_numero")

istr_mant.Argumento[17] = '0'
pb_copiar.Enabled = TRUE

HabilitaEncab(True)

end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra			= False
istr_mant.agrega			= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled		= True
END IF
pb_eli_det.Enabled	= True

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)




end event

event open;Datawindow dw_data 
string ls_select 
/*Argumentos
istr_mant.argumento[1]  = planilla
istr_mant.argumento[2]  = nave
istr_mant.argumento[3]  = recibidor
istr_mant.argumento[4]  = inspector
istr_mant.argumento[5]  = puerto
istr_mant.argumento[6]  = cliente
istr_mant.argumento[7]  = dw_1.Retrieve > 0 => 1 ; 0
istr_mant.argumento[8]  = especie
istr_mant.Argumento[9]  = Tipo de Ingreso <consulta o ingreso>
istr_mant.Argumento[10] = variedad
istr_mant.Argumento[11] = productor
istr_mant.Argumento[12] = calibre
istr_mant.Argumento[13] = etiqueta
istr_mant.Argumento[14] = embalaje
istr_mant.Argumento[15] = mercado
istr_mant.Argumento[16] = Destino
istr_mant.Argumento[17] = 0 = Recupera datos ;1 = copia datos al encabezado
istr_mant.Argumento[18] = tipo transporte
istr_mant.Argumento[19] = bodega 1
istr_mant.Argumento[20] = bodega 2
istr_mant.Argumento[21] = bodega 3
istr_mant.Argumento[22] = bodega 4
istr_mant.Argumento[23] = bodega 5
istr_mant.Argumento[24] = contenedor
istr_mant.Argumento[25] = lugar inspección
istr_mant.Argumento[26] = fecha arribo
istr_mant.Argumento[27] = fecha descarga
istr_mant.Argumento[28] = fecha inspección
istr_mant.Argumento[29] = fecha fumigación
istr_mant.Argumento[30] = reclamo
*/
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
This.ParentWindow().WindowState		=	Maximized!
This.WindowState							=	Maximized!
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

istr_busq	= Message.PowerObjectParm			

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)

istr_mant.dw						=	dw_1
istr_Mant.dw2						=	dw_2

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

//pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

buscar	= "Código:Ncodigo,Descripción:Sconcepto"
ordenar	= "Código:codigo,Descripción:concepto"

iuo_especie          		   = CREATE uo_especie
iuo_naves            		   = CREATE uo_naves
iuo_recibimercado    		   = CREATE uo_recibimercado
iuo_ctlcalinspectores		   = CREATE uo_ctlcalinspectores
iuo_puertos           		   = CREATE uo_puertos
iuo_variedades        		   = CREATE uo_variedades
iuo_productores       		   = CREATE uo_productores
iuo_calibre           		   = CREATE uo_calibre
iuo_embalajes         		   = CREATE uo_embalajesprod
iuo_etiquetas         		   = CREATE uo_etiquetas
iuo_destinos          		   = CREATE uo_destinos
iuo_mercado                   = CREATE uo_mercado
iuo_sitio                     = CREATE uo_sitioinspeccion

//Cliente
dw_2.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
IF idwc_clientes.Retrieve() = 0 THEN
	idwc_clientes.InsertRow(0)
END IF
dw_2.SetItem(1,"clie_codigo",gi_CodExport)

//Nave
dw_2.GetChild("nave_codigo", idwc_nave)
idwc_nave.SetTransObject(sqlca)
IF idwc_nave.Retrieve('M') = 0 THEN
	idwc_nave.InsertRow(0)
END IF
idwc_nave.SetSort("nave_nombre A")
idwc_nave.Sort()

//Recibidor
dw_2.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
IF idwc_recibidor.Retrieve(1)= 0 THEN//filtra por mercado
	idwc_recibidor.InsertRow(0)
END IF
idwc_recibidor.SetSort("reci_nombre A")
idwc_recibidor.Sort()

//Puerto
dw_2.GetChild("puer_codigo", idwc_puerto)
idwc_puerto.SetTransObject(sqlca)
IF idwc_puerto.Retrieve()= 0 THEN
	idwc_puerto.InsertRow(0)
END IF
idwc_puerto.SetSort("puer_nombre A")
idwc_puerto.Sort()

//Inspector
dw_2.GetChild("ccin_codigo", idwc_inspector)
ls_select = 'select * from dba.ctlcalinspectores where ccin_tipins = 2 OR ccin_tipins = 3' 
idwc_inspector.SetTransObject(sqlca)
idwc_inspector.setsqlselect (ls_select) 
IF idwc_inspector.Retrieve()= 0 THEN
	idwc_inspector.InsertRow(0)
END IF

//Especie
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
dw_2.SetItem(1,"espe_codigo",11)
IF idwc_especie.Retrieve()= 0 THEN
	idwc_especie.InsertRow(0)
END IF

//Mercado
dw_2.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
dw_2.SetItem(1,"merc_codigo",1)
IF idwc_mercado.Retrieve()= 0 THEN
	idwc_mercado.InsertRow(0)
END IF

//Productor
dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
IF idwc_productor.Retrieve()= 0 THEN
	idwc_productor.InsertRow(0)
END IF
idwc_productor.SetSort("prod_nombre A")
idwc_productor.Sort()

//Calibre
dw_2.GetChild("cpde_calibr", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
IF idwc_calibre.Retrieve(0,0)= 0 THEN
	idwc_calibre.InsertRow(0)
END IF

//Variedad
dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
IF idwc_variedad.Retrieve(11)= 0 THEN
	idwc_variedad.InsertRow(0)
END IF

//Embalaje
dw_2.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
IF idwc_embalaje.Retrieve(Gi_CodExport)= 0 THEN
	idwc_embalaje.InsertRow(0)
END IF
idwc_embalaje.SetFilter("Mid(emba_codigo,1,1) = 'U'")
idwc_embalaje.Filter()
idwc_embalaje.SetSort("emba_codigo A")
idwc_embalaje.Sort()

//Etiqueta
dw_2.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
IF idwc_etiqueta.Retrieve()= 0 THEN
	idwc_etiqueta.InsertRow(0)
END IF

//Destino
dw_2.GetChild("dest_codigo", idwc_destinos)
idwc_destinos.SetTransObject(sqlca)
dw_2.SetItem(1,"dest_codigo",225)
IF idwc_destinos.Retrieve(1)= 0 THEN//filtra por mercado
	idwc_destinos.InsertRow(0)
END IF

//Sitio de Inspección
dw_2.GetChild("ccsi_codigo", idwc_sitio)
idwc_sitio.SetTransObject(sqlca)
IF idwc_sitio.Retrieve()= 0 THEN
	idwc_sitio.InsertRow(0)
END IF

dw_2.SetItem(1,"cpde_reclam",0)
dw_2.SetItem(1,"nave_tipotr",'M')

istr_mant.argumento[1] = '0'
istr_mant.argumento[2] = ''
istr_mant.argumento[3] = '0'
istr_mant.argumento[4] = '0'
istr_mant.argumento[5] = '0'
istr_mant.argumento[6] = String(gi_CodExport)
istr_mant.argumento[7] = '0'
istr_mant.argumento[8] = String(gi_CodEspecie)
istr_Mant.argumento[15] = '1'
istr_Mant.argumento[16] = '225'
istr_Mant.argumento[17] = '0'

dw_2.SetFocus()
dw_2.SetColumn("cpde_numero")

istr_mant.Argumento[9] = istr_busq.Argum[4]
IF istr_busq.Argum[4] = 'consulta' THEN
	soloconsulta()
	istr_mant.Argumento[6]  = istr_busq.Argum[3]
	istr_mant.Argumento[1]  = istr_busq.Argum[1]
	istr_mant.Argumento[15] = istr_busq.Argum[5]
	istr_mant.Argumento[8]  = istr_busq.Argum[2]
	istr_mant.Argumento[16] = istr_busq.Argum[6]
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount()>0 THEN
	istr_mant.Agrega = False
	istr_mant.Borra  = False	
	OpenWithParm(iw_mantencion,istr_mant)
END IF



end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False

		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
 IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME INSPECCION DE UVA DE MESA EN DESTINO"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject	=	"dw_info_ctlcalplanillainspdestino"

vinf.dw_1.SetTransObject(sqlca)


fila	=	vinf.dw_1.Retrieve(Long(istr_mant.argumento[6]), Integer(istr_mant.argumento[1]),&
                            Integer(istr_mant.argumento[15]),Integer(istr_mant.argumento[8]),&
									 Integer(istr_mant.argumento[16]))

IF fila	=	-1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila	=	0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 80
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;Integer  li_secuencia, li_cliente, li_especie, li_variedad, li_etiqueta, li_mercado,&
         li_nave, li_destino
Long		ll_Fila, ll_numero, ll_lote, ll_productor
String   ls_calibre, ls_embalaje, ls_tipotra
Date     ld_fechain 

ll_numero      =  dw_2.Object.cpde_numero[1]
li_cliente		=	dw_2.Object.clie_codigo[1]
li_especie     =  dw_2.Object.espe_codigo[1]
ll_lote        =  dw_2.Object.cpde_tamlot[1]
ll_productor   =  dw_2.Object.prod_codigo[1]
ls_calibre     =  dw_2.Object.cpde_calibr[1]
li_variedad    =  dw_2.Object.vari_codigo[1]
ls_embalaje    =  dw_2.Object.emba_codigo[1]
li_etiqueta    =  dw_2.Object.etiq_codigo[1]
li_mercado     =  dw_2.Object.merc_codigo[1]
ld_fechain     =  dw_2.Object.cpde_fecins[1]
ls_tipotra     =  dw_2.Object.nave_tipotr[1]
li_nave        =  dw_2.Object.nave_codigo[1]
li_destino     =  dw_2.Object.dest_codigo[1]

IF dw_2.GetItemStatus(1,0,Primary!) = New! OR &
	dw_2.GetItemStatus(1,0,Primary!) = NewModified! OR istr_mant.argumento[17] = '1' THEN
	
	SELECT	IsNull(Max(cpde_numero),0)
		INTO  :ll_numero
		FROM  dba.ctlcalplandestinosenc
		WHERE clie_codigo = :li_cliente
		AND   espe_codigo = :li_especie
		AND   merc_codigo = :li_mercado
		AND   dest_codigo = :li_destino;
	
	ll_numero ++
	dw_2.Object.cpde_numero[1] = ll_numero
END IF	

SELECT	IsNull(Max(cpdd_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.ctlcalplandestinosdet
	WHERE	cpde_numero	=	:ll_Numero 
	AND	clie_codigo =	:li_cliente
	AND   espe_codigo =  :li_especie
	AND   merc_codigo =  :li_mercado
	AND   dest_codigo =  :li_destino;
	

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		li_Secuencia ++
		dw_1.Object.cpde_numero[ll_Fila] =	ll_Numero
		dw_1.Object.clie_codigo[ll_Fila] =	li_cliente
		dw_1.Object.cpdd_secuen[ll_Fila] =	li_Secuencia
		dw_1.Object.espe_codigo[ll_Fila] =  li_especie
		dw_1.Object.merc_codigo[ll_Fila] =  li_mercado
		dw_1.Object.dest_codigo[ll_Fila] =  li_destino
		dw_1.Object.cpde_fecins[ll_Fila] =  ld_fechain
		dw_1.Object.nave_tipotr[ll_Fila] =  ls_tipotra
		dw_1.Object.nave_codigo[ll_Fila] =  li_nave
		dw_1.Object.cpdd_tamlot[ll_Fila] =  ll_lote
		dw_1.Object.prod_codigo[ll_Fila] =  ll_productor
		dw_1.Object.cpdd_calibr[ll_Fila] =  ls_calibre
		dw_1.Object.vari_codigo[ll_Fila] =  li_variedad
		dw_1.Object.emba_codigo[ll_Fila] =  ls_embalaje
		dw_1.Object.etiq_codigo[ll_Fila] =  li_etiqueta
	END IF
NEXT
end event

event close;call super::close;This.ParentWindow().WindowState		=	Normal!

end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y + 10
	pb_buscar.width		= 235
	pb_buscar.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_copiar.Visible THEN
	pb_copiar.x				= li_posic_x
	pb_copiar.y				= li_posic_y + 20
	pb_copiar.width		= 235
	pb_copiar.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y + 30
	pb_nuevo.width		= 235
	pb_nuevo.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y + 40
	pb_eliminar.width		= 235
	pb_eliminar.height	  	= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y + 50
	pb_grabar.width		= 235
	pb_grabar.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y + 60
	pb_imprimir.width		= 235
	pb_imprimir.height	= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y + 70
	pb_salir.width			= 235
	pb_salir.height			= 195
	li_visible ++
	li_posic_y += 180
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 133
pb_ins_det.width		= 235
pb_ins_det.height		= 195

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 220
pb_eli_det.width		= 235
pb_eli_det.height		= 195
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	habilitaencab(False)
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_planillainspecdestino
integer x = 46
integer y = 940
integer width = 4626
integer height = 1248
string title = "Detalle Planilla  de Inspección de Uva de Mesa en Destino"
string dataobject = "dw_mant_mues_planillainspdestinodet"
boolean hscrollbar = false
boolean hsplitscroll = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_planillainspecdestino
integer x = 251
integer y = 32
integer width = 4219
integer height = 888
string dataobject = "dw_mant_mues_planillainspdestinoenc"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula, ls_NroGuia


SetNull(ls_Nula)

ls_Columna	=	dwo.Name

istr_mant.argumento[8] = '11'//Especie

CHOOSE CASE ls_Columna
		
	CASE "clie_codigo"
		   idwc_embalaje.Retrieve(Integer(data))
		   istr_mant.argumento[6] = data
			
	CASE "cpde_numero"
		IF Trae_Planilla(Long(data),dw_2.Object.clie_codigo[1],Integer(istr_mant.argumento[8]),&
			              dw_2.Object.merc_codigo[1],dw_2.Object.dest_codigo[1]) THEN
			istr_mant.argumento[1] = data		
		ELSE
			This.SetItem(1,ls_Columna,Long(ls_nula))
			RETURN 1
		END IF
			
	CASE "vari_codigo"
		IF NOT iuo_variedades.Existe(Integer(istr_mant.argumento[8]),Integer(Data),True,SqlCa) THEN
			This.SetItem(1,ls_Columna,Integer(ls_nula))
			RETURN 1
		ELSE
			istr_mant.argumento[10] = Data
			idwc_calibre.Retrieve(Integer(istr_mant.argumento[8]),Integer(Data))
		END IF
		
	CASE "nave_tipotr"
		   dw_2.GetChild("nave_codigo", idwc_nave)
         idwc_nave.SetTransObject(sqlca)
         idwc_nave.Retrieve(data)
			dw_2.SetItem(1, "nave_codigo", Integer(ls_nula))
								
	CASE "nave_codigo"
		  IF IsNull(dw_2.Object.nave_tipotr[1]) OR dw_2.Object.nave_tipotr[1] = '' THEN
			  Messagebox("Atención","Debe Seleccionar Tipo de Nave",exclamation!)
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSEIF NoT iuo_Naves.Existe(Integer(data),dw_2.Object.nave_tipotr[1],True,SqlCa) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[2] = data
		  END IF
		  
	CASE "merc_codigo"
		  IF NOT iuo_mercado.Existe(Integer(Data),True,SqlCa) THEN 
				This.SetItem(1,ls_Columna,Integer(ls_nula))
			ELSE
				istr_mant.Argumento[15] = Data
				dw_2.GetChild("reci_codigo", idwc_recibidor)
				idwc_recibidor.SetTransObject(sqlca)
				dw_2.SetItem(1,"reci_codigo",Integer(ls_nula))
				idwc_recibidor.Retrieve(Integer(data))
				
				dw_2.GetChild("dest_codigo", idwc_destinos)
				idwc_destinos.SetTransObject(sqlca)
				dw_2.SetItem(1,"dest_codigo",Integer(ls_nula))
				idwc_destinos.Retrieve(Integer(data))
			END IF

	CASE "reci_codigo"
		  IF IsNull(dw_2.Object.merc_codigo[1]) OR dw_2.Object.merc_codigo[1] = 0 THEN
			  Messagebox("Atención","Debe Seleccionar Mercado",exclamation!)
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		   ELSEIF NOT iuo_recibimercado.Existe(dw_2.Object.Merc_codigo[1],Long(data),True,SqlCa) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
		     istr_mant.argumento[3] = data 
	     END IF
		  
	CASE "dest_codigo"
		IF NOT iuo_destinos.Existe(dw_2.Object.merc_codigo[1],Integer(Data),True,SqlCa) THEN
			This.SetItem(1,ls_Columna,Integer(ls_nula))
			RETURN 1
		ELSE
			istr_mant.argumento[16] = Data
		END IF
		  

	CASE "puer_codigo"
		  IF Not iuo_puertos.Existe(Integer(data),True,SqlCa) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[5] = data
			  dw_2.GetChild("puer_codigo", idwc_puerto)
           idwc_puerto.SetTransObject(sqlca)
           idwc_puerto.Retrieve(0)
	     END IF
		  
	CASE "prod_codigo"
		  IF NOT iuo_productores.Existe(Long(Data),True,SqlCa) THEN
			  This.SetItem(1,ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
			istr_mant.argumento[11] = Data
		  END IF
		  
	CASE "emba_codigo"
		 IF NOT iuo_embalajes.Existe(Integer(istr_mant.argumento[6]),Data,True,SqlCa) THEN
			This.SetItem(1,ls_Columna,ls_nula)
			RETURN 1
		ELSE
			istr_mant.argumento[14] = Data
		END IF
		
	CASE "cpde_calibr"
		IF NOT iuo_calibre.Existe(Integer(istr_mant.argumento[8]),This.Object.vari_codigo[1],&
		                          Data,True,SqlCa) THEN
			This.SetItem(1,ls_Columna,ls_nula)
			RETURN 1
		ELSE
			istr_mant.argumento[12] = Data
		END IF
		
	CASE "etiq_codigo"
		IF NOT iuo_etiquetas.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1,ls_Columna,Integer(ls_nula))
			RETURN 1
		ELSE
			istr_mant.argumento[13] = Data
		END IF
		
	CASE "ccin_codigo"
		IF NOT iuo_ctlcalinspectores.Existe(SqlCa,Integer(Data),True) THEN
			This.SetItem(1,ls_Columna,Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "ccsi_codigo"
		IF NOT iuo_sitio.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1,ls_Columna,Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "cpde_fecarr"
		istr_mant.argumento[26] = Data
		This.SetItem(1,"cpde_fecdes",Date(Data))
		This.SetItem(1,"cpde_fecins",Date(Data))
		This.SetItem(1,"cpde_fecfum",Date(Data))
		
	CASE "cpde_fecdes"
		istr_mant.argumento[27] = Data
		
	CASE "cpde_fecins"
		istr_mant.argumento[28] = Data
		
	CASE "cpde_fecfum"
		istr_mant.argumento[29] = Data	
	
END CHOOSE

habilitaingreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_planillainspecdestino
integer x = 4722
integer y = 540
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_planillainspecdestino
integer x = 4722
integer y = 720
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_planillainspecdestino
integer x = 4722
integer y = 904
end type

event pb_grabar::clicked;call super::clicked;//DwItemStatus	Estado, estado1
//Integer ll,li
//
//Estado	=	dw_2.GetItemStatus(il_fila, 0, Primary!)
////Estado1	=	dw_1.GetItemStatus(il_fila, 0, Primary!)
//dw_1.reset()
//Estado1	=	dw_1.GetItemStatus(il_fila, 0, Primary!)
//
//if ll = 0 then
//	li = 1
//end if
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_planillainspecdestino
integer x = 4722
integer y = 1080
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_planillainspecdestino
integer x = 4722
integer y = 1260
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_planillainspecdestino
integer x = 4722
integer y = 1572
integer taborder = 100
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_planillainspecdestino
integer x = 4722
integer y = 1744
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_planillainspecdestino
integer x = 4722
integer y = 204
end type

event pb_buscar::clicked;istr_mant.Argumento[17] = '0'
pb_copiar.Enabled = FALSE
Parent.TriggerEvent("ue_seleccion")
end event

type pb_copiar from picturebutton within w_maed_planillainspecdestino
string tag = "Copia Datos al Encabezado"
integer x = 4722
integer y = 368
integer width = 233
integer height = 196
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "Copy!"
string disabledname = "Copy!"
alignment htextalign = left!
end type

event clicked;String ls_nula
SetNull(ls_nula)
istr_mant.Argumento[17] = '1'
IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN 
	Parent.TriggerEvent("ue_seleccion")
ELSE
	dw_1.Reset()
	CopiaEncabezado()
	habilitaencab(True)
	pb_grabar.Enabled  = False
END IF
end event

