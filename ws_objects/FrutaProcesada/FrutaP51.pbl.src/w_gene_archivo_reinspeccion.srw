$PBExportHeader$w_gene_archivo_reinspeccion.srw
forward
global type w_gene_archivo_reinspeccion from window
end type
type dw_5 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_7 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_8 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_3 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_4 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_6 from datawindow within w_gene_archivo_reinspeccion
end type
type cbx_terceros from checkbox within w_gene_archivo_reinspeccion
end type
type st_7 from statictext within w_gene_archivo_reinspeccion
end type
type em_fecha from editmask within w_gene_archivo_reinspeccion
end type
type dw_11 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_10 from datawindow within w_gene_archivo_reinspeccion
end type
type st_4 from statictext within w_gene_archivo_reinspeccion
end type
type st_3 from statictext within w_gene_archivo_reinspeccion
end type
type sle_mensa from singlelineedit within w_gene_archivo_reinspeccion
end type
type em_inspeccion from editmask within w_gene_archivo_reinspeccion
end type
type st_5 from statictext within w_gene_archivo_reinspeccion
end type
type st_2 from statictext within w_gene_archivo_reinspeccion
end type
type st_1 from statictext within w_gene_archivo_reinspeccion
end type
type pb_salir from picturebutton within w_gene_archivo_reinspeccion
end type
type pb_grabar from picturebutton within w_gene_archivo_reinspeccion
end type
type gb_2 from groupbox within w_gene_archivo_reinspeccion
end type
type gb_1 from groupbox within w_gene_archivo_reinspeccion
end type
type st_6 from statictext within w_gene_archivo_reinspeccion
end type
type dw_2 from datawindow within w_gene_archivo_reinspeccion
end type
type dw_1 from datawindow within w_gene_archivo_reinspeccion
end type
end forward

global type w_gene_archivo_reinspeccion from window
integer width = 2359
integer height = 1336
boolean titlebar = true
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
event type long ue_despuesguardar ( )
dw_5 dw_5
dw_7 dw_7
dw_8 dw_8
dw_3 dw_3
dw_4 dw_4
dw_6 dw_6
cbx_terceros cbx_terceros
st_7 st_7
em_fecha em_fecha
dw_11 dw_11
dw_10 dw_10
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_inspeccion em_inspeccion
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
st_6 st_6
dw_2 dw_2
dw_1 dw_1
end type
global w_gene_archivo_reinspeccion w_gene_archivo_reinspeccion

type variables
str_mant               istr_mant
Date		id_FechaAcceso
Time		it_HoraAcceso
Integer	ii_estado

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function boolean existeinspeccion (long al_inspec)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_numinpe
Integer		li_PldSag, li_cliente, li_planta
String		ls_Archivo, ls_Registro

dw_2.reset()
dw_1.reset()

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)

ll_Numero	= Long(em_inspeccion.text)
li_planta 	= Integer(istr_mant.argumento[2])
li_cliente 	= Integer(istr_mant.argumento[1])

IF ii_estado = 5 THEN
	MessageBox("Atención", "Esta Re-Inspección Se encuentra con Estado Pendiente.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
	Return
END IF						


IF ii_estado <> 2 OR isnull(ii_estado) THEN

	ll_Filas		= dw_2.Retrieve(2,ll_Numero,Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]))
	
	IF ll_Filas = -1 THEN
		F_ErrorBaseDatos(sqlca,"Recuperación datos de Re-Inspección")
	ELSEIF ll_Filas = 0 THEN
		MessageBox("Atención", "No hay información para Re-Inspección Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_inspeccion.SetFocus()
	ELSE
		
		dw_2.SetSort('clie_codigo,paen_numero')
		dw_2.Sort()
	
		li_PldSag	=	dw_2.Object.plde_codpla[1]
		
		ls_Registro	=	String(ll_Numero,'00000')
		ls_Registro +=	String(li_PldSag,'0000')
		ls_Registro	+= String(dw_2.Object.cant_pallet[1], '0000')
		
		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	
		FOR ll_Fila = 1 TO ll_Filas
	
	//		ls_Registro =	String(Integer(istr_mant.argumento[1]),'000')
			ls_Registro =	String(dw_2.Object.clie_codigo[ll_fila],'000')
			ls_Registro	+=	String(dw_2.Object.paen_numero[ll_Fila], '0000000')
			ls_Registro	+=	String(dw_2.Object.inpd_fechai[ll_Fila], 'YYYYMMDD')
				
			ll_FilaDet	=	dw_1.InsertRow(0)
			dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
		NEXT
	
		ls_Registro	= '&&'
	
		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
		
		ls_Archivo	= String(li_PldSag,'000') + String(ll_Numero,'00000') + ".RIN"
	
		IF dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 THEN
			MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
		ELSE
			sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
			
			ll_numinpe = Long(em_inspeccion.Text)
			
			UPDATE dba.inspecpalenc SET
			inpe_estado = 2
			WHERE inpe_numero = :ll_numinpe
			AND	:li_cliente in (-1,clie_codigo)
			AND	plde_codigo = :li_planta
			USING Sqlca;
			
			//This.TriggerEvent("ue_despuesguardar")
		END IF
		
		dw_2.Reset()
	
	END IF
	
	em_inspeccion.SetFocus()
ELSE
	MessageBox("Atención", "Esta Re-Inspección Se encuentra Cerrada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
						Return
END IF	
end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event type long ue_despuesguardar();SetPointer(HourGlass!)

Long		ll_Filas, ll_fila1, ll_numero, ll_nueva, ll_nueva1, ll_fila, ll_nueva2,&
			ll_fila2, ll_nueva3, ll_fila3, fila_find, ll_pallet, fila_find2, ll_numinpe
Integer	li_Cliente, li_planta
Boolean	lb_AutoCommit, lb_Retorno

IF em_inspeccion.Text = "" THEN 
	MessageBox( "Advertencia", "Falta Número de Inspección.", &
					StopSign!, Ok!)
	RETURN 1
ELSE
	ll_numinpe = Long(em_inspeccion.Text)
END IF	

ll_Filas = dw_7.Retrieve(Integer(istr_mant.argumento[1]), &
								Integer(istr_mant.argumento[2]), &
								ll_numinpe,2)
								
dw_8.Retrieve(Integer(istr_mant.argumento[1]), &
								Integer(istr_mant.argumento[2]), &
								ll_numinpe,2)
								
IF ll_Filas = -1 THEN								
MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
					Return 1
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
					Return 1
END IF	

li_cliente = Integer(istr_mant.Argumento[1])
li_planta =	 Integer(istr_mant.Argumento[2])

Select max(altu_numero) + 1
into :ll_numero
from dba.alpalletencab
where clie_codigo = :li_cliente
and	plde_codigo = :li_planta;

IF isnull(dw_7.Object.inpe_estado[1]) OR dw_7.Object.inpe_estado[1] = 0 THEN
	
	ll_nueva = dw_3.InsertRow(0)
	
	dw_3.Object.clie_codigo[ll_nueva] = li_cliente  
	dw_3.Object.plde_codigo[ll_nueva] = li_planta
	dw_3.Object.altu_numero[ll_nueva] = ll_numero
	dw_3.Object.altu_fecmov[ll_nueva] = dw_7.Object.paen_fecemb[1]
	dw_3.Object.altu_observ[ll_nueva] = 'Cierre de Inspección'

END IF

FOR ll_fila1 = 1 TO dw_8.RowCount() 
	IF isnull(dw_7.Object.inpe_estado[1]) OR dw_7.Object.inpe_estado[1] = 0 THEN
		
		ll_pallet = dw_8.Object.paen_numero[ll_fila1]

		fila_find2 =  dw_4.Find("paen_numero = " + String(ll_pallet),&
			1, dw_4.RowCount())
			
		IF fila_find2 = 0 THEN 
		
			ll_nueva1 = dw_4.InsertRow(0)
			
			dw_4.Object.clie_codigo[ll_nueva1] = li_cliente  
			dw_4.Object.plde_codigo[ll_nueva1] = li_planta
			dw_4.Object.altu_numero[ll_nueva1] = ll_numero
			dw_4.Object.alpf_fecmov[ll_nueva1] = dw_8.Object.pafr_fecemb[ll_fila1]
			dw_4.Object.paen_numero[ll_nueva1] = dw_8.Object.paen_numero[ll_fila1]
		END IF			
	END IF
NEXT

FOR ll_fila2 = 1 TO dw_7.RowCount() 
	IF isnull(dw_7.Object.inpe_estado[1]) OR dw_7.Object.inpe_estado[1] = 0 THEN
		
		ll_nueva2 = dw_5.InsertRow(0)
		
		dw_5.Object.clie_codigo[ll_nueva2] = li_cliente   
		dw_5.Object.paen_numero[ll_nueva2] = dw_7.Object.paen_numero[ll_fila2]  
		dw_5.Object.plde_codigo[ll_nueva2] = li_planta  
		dw_5.Object.pahi_proces[ll_nueva2] = ll_numero  
		dw_5.Object.pahi_tipopa[ll_nueva2] = dw_7.Object.paen_tipopa[ll_fila2]  
		dw_5.Object.tpem_codigo[ll_nueva2] = dw_7.Object.tpem_codigo[ll_fila2]     
		dw_5.Object.espe_codigo[ll_nueva2] = dw_7.Object.espe_codigo[ll_fila2]    
		dw_5.Object.vari_codigo[ll_nueva2] = dw_7.Object.vari_codigo[ll_fila2]    
		dw_5.Object.tiem_codigo[ll_nueva2] = dw_7.Object.tiem_codigo[ll_fila2]    
		dw_5.Object.emba_codigo[ll_nueva2] = dw_7.Object.emba_codigo[ll_fila2]    
		dw_5.Object.cate_codigo[ll_nueva2] = dw_7.Object.cate_codigo[ll_fila2]    
		dw_5.Object.etiq_codigo[ll_nueva2] = dw_7.Object.etiq_codigo[ll_fila2]    
		dw_5.Object.stat_codigo[ll_nueva2] = dw_7.Object.stat_codigo[ll_fila2]    
		dw_5.Object.trat_codigo[ll_nueva2] = dw_7.Object.trat_codigo[ll_fila2]    
		dw_5.Object.frio_codigo[ll_nueva2] = dw_7.Object.frio_codigo[ll_fila2]    
		dw_5.Object.cond_codigo[ll_nueva2] = dw_7.Object.cond_codigo[ll_fila2]    
		dw_5.Object.dest_codigo[ll_nueva2] = dw_7.Object.dest_codigo[ll_fila2]    
		dw_5.Object.pahi_fecemb[ll_nueva2] = dw_7.Object.paen_fecemb[ll_fila2]    
		dw_5.Object.pahi_cosecha[ll_nueva2] = dw_7.Object.paen_cosecha[ll_fila2]     
		dw_5.Object.paen_altura[ll_nueva2] = dw_7.Object.paen_altura[ll_fila2]    
		dw_5.Object.paen_ccajas[ll_nueva2] = dw_7.Object.paen_ccajas[ll_fila2]    
		dw_5.Object.tmvp_codigo[ll_nueva2] = dw_7.Object.tmvp_codigo[ll_fila2]    
		dw_5.Object.paen_fecini[ll_nueva2] = dw_7.Object.paen_fecini[ll_fila2]    
		dw_5.Object.paen_horain[ll_nueva2] = dw_7.Object.paen_horain[ll_fila2]    
		dw_5.Object.cama_codigo[ll_nueva2] = dw_7.Object.cama_codigo[ll_fila2]    
		dw_5.Object.pahi_calle[ll_nueva2] = dw_7.Object.paen_calle[ll_fila2]    
		dw_5.Object.pahi_base[ll_nueva2] = dw_7.Object.paen_base[ll_fila2]   
		dw_5.Object.pahi_posici[ll_nueva2] = dw_7.Object.paen_posici[ll_fila2]    
		dw_5.Object.pahi_estado[ll_nueva2] = dw_7.Object.paen_estado[ll_fila2]    
		dw_5.Object.pahi_inspec[ll_nueva2] = dw_7.Object.paen_inspec[ll_fila2]    
		dw_5.Object.pahi_concal[ll_nueva2] = dw_7.Object.paen_concal[ll_fila2]    
		dw_5.Object.pahi_pexpor[ll_nueva2] = dw_7.Object.paen_pexpor[ll_fila2]
		dw_5.Object.pahi_pmixto[ll_nueva2] = dw_7.Object.paen_pmixto[ll_fila2]

	END IF
NEXT

FOR ll_fila3 = 1 TO dw_8.RowCount() 
	IF isnull(dw_7.Object.inpe_estado[1]) OR dw_7.Object.inpe_estado[1] = 0 THEN
		
		ll_nueva3 = dw_5.InsertRow(0)

		dw_6.Object.clie_codigo[ll_fila3] = li_cliente 
		dw_6.Object.paen_numero[ll_fila3] = dw_8.Object.paen_numero[ll_fila3]    
		dw_6.Object.espe_codigo[ll_fila3] = dw_8.Object.espe_codigo[ll_fila3]  
		dw_6.Object.vari_codigo[ll_fila3] = dw_8.Object.vari_codigo[ll_fila3]    
		dw_6.Object.emba_codigo[ll_fila3] = dw_8.Object.emba_codigo[ll_fila3]    
		dw_6.Object.prod_codigo[ll_fila3] = dw_8.Object.prod_codigo[ll_fila3]    
		dw_6.Object.cond_codigo[ll_fila3] = dw_8.Object.cond_codigo[ll_fila3]    
		dw_6.Object.etiq_codigo[ll_fila3] = dw_8.Object.etiq_codigo[ll_fila3]     
		dw_6.Object.plde_codigo[ll_fila3] = li_planta  
		dw_6.Object.pafh_calibr[ll_fila3] = dw_8.Object.pafr_calibr[ll_fila3]    
		dw_6.Object.pafh_proces[ll_fila3] = ll_numero  
		dw_6.Object.pafh_secuen[ll_fila3] = dw_8.Object.pafr_secuen[ll_fila3]    
		dw_6.Object.pafh_ccajas[ll_fila3] = dw_8.Object.pafr_ccajas[ll_fila3]    
		dw_6.Object.pafh_nrlote[ll_fila3] = dw_8.Object.pafr_nrlote[ll_fila3]    
		dw_6.Object.pafr_fecing[ll_fila3] = dw_8.Object.pafr_fecing[ll_fila3]    
		dw_6.Object.pafr_fecemb[ll_fila3] = dw_8.Object.pafr_fecemb[ll_fila3]    
		dw_6.Object.pafr_copack[ll_fila3] = dw_8.Object.pafr_copack[ll_fila3]    
		dw_6.Object.pafr_tipdoc[ll_fila3] = 7 
		dw_6.Object.pafr_huert1[ll_fila3] = dw_8.Object.pafr_huert1[ll_fila3]  
		dw_6.Object.pafr_cuart1[ll_fila3] = dw_8.Object.pafr_cuart1[ll_fila3] 
			
	END IF
NEXT

IF dw_3.Rowcount() > 0 THEN
	lb_AutoCommit		=	sqlca.AutoCommit
	sqlca.AutoCommit	=	False
	
	IF dw_5.Update(True, False) = 1 THEN
		IF dw_6.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
				//		F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
						Return 1
					ELSE
						lb_Retorno	=	True
						
						dw_5.ResetUpdate()
						dw_6.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
					END IF
					
				ELSE	
						F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
					Return 1
				END IF	
			ELSE	
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
					Return 1
			END IF		
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
			Return 1
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
			Messagebox("Inpección","Mo se Pudo grabar registro en tablas Históricas")
			Return 1
		END IF
		
		Messagebox("Inpección","Grabación de Datos realizada Satisfactoriamente")
				
		Update dba.inspecpalenc Set
		inpe_estado = 1,
		inpe_proces = :ll_numero
		where inpe_numero = :ll_numinpe
		and	clie_codigo = :li_cliente
		and	plde_codigo = :li_planta
		Using Sqlca;
		
		
		sqlca.AutoCommit	=	lb_AutoCommit
		
	//	Return 1
END IF

Return 0

end event

public function boolean existeinspeccion (long al_inspec);Integer	li_codexp, li_planta
Date		ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_inspec <> 0 OR li_planta = 0 THEN
	
	SELECT	inpe_fechai, inpe_estado
		INTO	:ld_fecha, :ii_estado
		FROM	dba.INSPECPALENC
		WHERE	clie_codigo	=	:li_codexp
		AND	plde_codigo =	:li_planta
		AND	inpe_tipoin =	2
		AND	inpe_numero	=	:al_inspec ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla INSPECPALENC")
		em_inspeccion.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Nro. Re-Inspección Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_inspeccion.SetFocus()
		RETURN False
	ELSE
		em_fecha.text		= String(ld_fecha)
		sle_mensa.text		= ""
		pb_grabar.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_gene_archivo_reinspeccion.create
this.dw_5=create dw_5
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_6=create dw_6
this.cbx_terceros=create cbx_terceros
this.st_7=create st_7
this.em_fecha=create em_fecha
this.dw_11=create dw_11
this.dw_10=create dw_10
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_inspeccion=create em_inspeccion
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.st_6=create st_6
this.dw_2=create dw_2
this.dw_1=create dw_1
this.Control[]={this.dw_5,&
this.dw_7,&
this.dw_8,&
this.dw_3,&
this.dw_4,&
this.dw_6,&
this.cbx_terceros,&
this.st_7,&
this.em_fecha,&
this.dw_11,&
this.dw_10,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_inspeccion,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.st_6,&
this.dw_2,&
this.dw_1}
end on

on w_gene_archivo_reinspeccion.destroy
destroy(this.dw_5)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_6)
destroy(this.cbx_terceros)
destroy(this.st_7)
destroy(this.em_fecha)
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_inspeccion)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.st_6)
destroy(this.dw_2)
destroy(this.dw_1)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)
dw_10.SetItem(1,"clie_codigo",gi_codexport)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

istr_mant.argumento[1]	=	String(gi_codexport,'000')
istr_mant.argumento[2]	=	String(gi_codplanta)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
dw_7.SetTransObject(Sqlca)
dw_8.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
dw_4.SetTransObject(Sqlca)
dw_5.SetTransObject(Sqlca)
dw_6.SetTransObject(Sqlca)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_5 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 1778
integer y = 1236
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_palletencabhisto_inpe"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

type dw_7 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 933
integer y = 1212
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_palletencab_inpe"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

type dw_8 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 247
integer y = 1212
integer width = 686
integer height = 400
integer taborder = 40
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_palletfruta_inpe"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

type dw_3 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 229
integer y = 1628
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_alpalletencab"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

type dw_4 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 1010
integer y = 1640
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

type dw_6 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 1842
integer y = 1580
integer width = 686
integer height = 400
integer taborder = 40
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_palletfrutahisto_inpe"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

type cbx_terceros from checkbox within w_gene_archivo_reinspeccion
integer x = 402
integer y = 764
integer width = 1344
integer height = 116
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Incluye Terceros mismas Características"
end type

event clicked;IF This.Checked THEN
	dw_10.Enabled		=	False
	dw_10.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_10.Enabled		=	True
	dw_10.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[1]	=	String(dw_10.Object.clie_codigo[1])
END IF
end event

type st_7 from statictext within w_gene_archivo_reinspeccion
integer x = 82
integer y = 704
integer width = 1815
integer height = 256
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

type em_fecha from editmask within w_gene_archivo_reinspeccion
integer x = 1207
integer y = 540
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

type dw_11 from datawindow within w_gene_archivo_reinspeccion
integer x = 425
integer y = 416
integer width = 969
integer height = 100
integer taborder = 20
boolean bringtotop = true
string title = "none"
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

type dw_10 from datawindow within w_gene_archivo_reinspeccion
integer x = 425
integer y = 300
integer width = 1152
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(Integer(data),'000')
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type st_4 from statictext within w_gene_archivo_reinspeccion
integer x = 178
integer y = 440
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_reinspeccion
integer x = 178
integer y = 324
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archivo_reinspeccion
integer x = 178
integer y = 1028
integer width = 1632
integer height = 92
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

type em_inspeccion from editmask within w_gene_archivo_reinspeccion
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 754
integer y = 540
integer width = 443
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;IF ExisteInspeccion(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type st_5 from statictext within w_gene_archivo_reinspeccion
integer x = 78
integer y = 68
integer width = 1815
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Genera Archivo Plano Re-Inspección"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_archivo_reinspeccion
integer x = 178
integer y = 556
integer width = 558
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Nro. Re-Inspección"
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_reinspeccion
integer x = 82
integer y = 224
integer width = 1815
integer height = 480
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

type pb_salir from picturebutton within w_gene_archivo_reinspeccion
integer x = 1989
integer y = 964
integer width = 233
integer height = 196
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_reinspeccion
integer x = 1979
integer y = 712
integer width = 233
integer height = 196
integer taborder = 50
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

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_2 from groupbox within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 1957
integer y = 656
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
end type

type gb_1 from groupbox within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 1957
integer y = 916
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
end type

type st_6 from statictext within w_gene_archivo_reinspeccion
integer x = 82
integer y = 960
integer width = 1815
integer height = 224
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

type dw_2 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 110
integer y = 900
integer width = 1230
integer height = 328
string dataobject = "dw_gene_archivo_inspeccion_pro"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type dw_1 from datawindow within w_gene_archivo_reinspeccion
boolean visible = false
integer x = 1929
integer y = 1000
integer width = 352
integer height = 256
string dataobject = "dw_gene_archivo_saam_plano"
boolean livescroll = true
end type

