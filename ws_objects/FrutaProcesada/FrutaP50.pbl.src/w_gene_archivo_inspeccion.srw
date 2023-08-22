$PBExportHeader$w_gene_archivo_inspeccion.srw
forward
global type w_gene_archivo_inspeccion from window
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_inspeccion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_inspeccion
end type
type cbx_conex from checkbox within w_gene_archivo_inspeccion
end type
type dw_9 from datawindow within w_gene_archivo_inspeccion
end type
type dw_8 from datawindow within w_gene_archivo_inspeccion
end type
type dw_7 from datawindow within w_gene_archivo_inspeccion
end type
type dw_3 from datawindow within w_gene_archivo_inspeccion
end type
type dw_4 from datawindow within w_gene_archivo_inspeccion
end type
type dw_5 from datawindow within w_gene_archivo_inspeccion
end type
type dw_6 from datawindow within w_gene_archivo_inspeccion
end type
type cbx_var from checkbox within w_gene_archivo_inspeccion
end type
type cbx_terceros from checkbox within w_gene_archivo_inspeccion
end type
type st_7 from statictext within w_gene_archivo_inspeccion
end type
type em_fecha from editmask within w_gene_archivo_inspeccion
end type
type st_4 from statictext within w_gene_archivo_inspeccion
end type
type st_3 from statictext within w_gene_archivo_inspeccion
end type
type sle_mensa from singlelineedit within w_gene_archivo_inspeccion
end type
type em_inspeccion from editmask within w_gene_archivo_inspeccion
end type
type st_5 from statictext within w_gene_archivo_inspeccion
end type
type st_2 from statictext within w_gene_archivo_inspeccion
end type
type pb_salir from picturebutton within w_gene_archivo_inspeccion
end type
type pb_grabar from picturebutton within w_gene_archivo_inspeccion
end type
type st_6 from statictext within w_gene_archivo_inspeccion
end type
type st_1 from statictext within w_gene_archivo_inspeccion
end type
type dw_2 from datawindow within w_gene_archivo_inspeccion
end type
type dw_1 from datawindow within w_gene_archivo_inspeccion
end type
end forward

global type w_gene_archivo_inspeccion from window
integer x = 599
integer y = 828
integer width = 2423
integer height = 1284
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
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
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
cbx_conex cbx_conex
dw_9 dw_9
dw_8 dw_8
dw_7 dw_7
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
cbx_var cbx_var
cbx_terceros cbx_terceros
st_7 st_7
em_fecha em_fecha
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_inspeccion em_inspeccion
st_5 st_5
st_2 st_2
pb_salir pb_salir
pb_grabar pb_grabar
st_6 st_6
st_1 st_1
dw_2 dw_2
dw_1 dw_1
end type
global w_gene_archivo_inspeccion w_gene_archivo_inspeccion

type variables
Date		id_FechaAcceso
Time		it_HoraAcceso
Integer	ii_var, ii_estado

Transaction	Sqlprod

uo_odbc				iuo_odbc
end variables

forward prototypes
public function boolean existeinspeccion (long al_inspec)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_numinpe, ll_fil
Integer		li_PldSag, li_region, li_conexion
String			ls_Especie, ls_EspeAnt, ls_Archivo, ls_Registro, ls_rotulacion,&
				ls_conexion,ls_nomodb,ls_nomser,ls_nombas,li_base,ls_Usuario,ls_Password,&
				ls_ubicacion,ls_ip,ls_puerto,ls_nodbms,ls_Motor
Boolean		lb_Conectado

dw_2.reset()
dw_1.reset()

If Not DirectoryExists(gs_disco+":\GeneradosSAG") Then
	MessageBox('Error', 'El directorio para la generacion de archivo: ' + gs_disco+":\GeneradosSAG" + &
					"~n~nNo existe, favor revisar o comunicarse con informatica, para activar directorio.", StopSign!, Ok!)
	Return
End If

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)
dw_9.SetTransObject(Sqlca)

ll_Numero	= Long(em_inspeccion.text)

SELECT plde_region INTO :li_region
FROM dbo.plantadesp
WHERE plde_codigo=:uo_SelPlanta.Codigo;

If IsNull(li_region) Then 
	li_region=5 
End If

If ii_estado = 5 Then
	MessageBox("Atención", "la Inspección Ingresada se encuentra en Estado PEndiente.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
	Return 
End If	

If ii_estado <> 2 OR isnull(ii_estado) Then 
	ll_Filas		= dw_2.Retrieve(1,ll_Numero,uo_SelCliente.Codigo, uo_SelPlanta.Codigo)
	
	If cbx_terceros.Checked AND cbx_conex.Checked Then
		ll_fil = dw_9.Retrieve(1,-1) 
		If ll_fil > 0 Then
					
			Sqlprod	=	Create Transaction
			
			FOR ll_fila =  1 TO dw_9.RowCount() 
					
				li_conexion		=	dw_9.Object.cone_codigo[ll_fila]
				ls_conexion		=	dw_9.Object.cone_descri[ll_fila]
				ls_nomodb		=	dw_9.Object.cone_nomodb[ll_fila]
				ls_nomser		=	dw_9.Object.cone_nomser[ll_fila]
				ls_nombas   	=	dw_9.Object.cone_nombas[ll_fila]
				ls_nodbms		=	dw_9.Object.cone_nodbms[ll_fila]
				ls_Usuario		=	dw_9.Object.cone_nomusu[ll_fila]
				ls_Password		=	dw_9.Object.cone_passwo[ll_fila]	
				ls_ubicacion		=	dw_9.Object.cone_ubicac[ll_fila]
				ls_ip				=	dw_9.Object.cone_ipserv[ll_fila]
				ls_puerto			=	dw_9.Object.cone_puerto[ll_fila]	
						
				If lb_Conectado Then 
					DISCONNECT USING Sqlprod;
					lb_Conectado	=	False
				End If
				
				iuo_odbc			=	Create uo_odbc
						
				ls_Motor = iuo_Odbc.MotorBD()
				
				If ls_Motor = '' OR ls_ubicacion = '' OR ls_ip = '' OR ls_puerto = '' Then
					MessageBox("Atención", "Falta mantención a tabla conexiones.")
					lb_Conectado = False
				End If	
				
				iuo_Odbc.Crea(ls_nomodb, ls_Usuario, ls_Password,&
					 ls_nombas,ls_ubicacion,ls_nomser,ls_ip, &
					 ls_puerto, ls_motor)
			
				Sqlprod.ServerName	=	ls_nomser
				Sqlprod.DataBase		=	ls_nombas
				Sqlprod.Dbms			= 	ls_nodbms
				Sqlprod.DbParm			=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
												";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
			
				CONNECT USING Sqlprod;
				
				If Sqlprod.SQLCode = 0 Then 
					lb_Conectado = True
						
					dw_2.SetTransObject(Sqlprod)
				End If	
				
				If lb_Conectado = TRUE Then
					ll_Filas		= dw_2.Retrieve(1,ll_Numero,uo_SelCliente.Codigo,uo_SelPlanta.Codigo)
				End If
				
				If lb_Conectado Then 
					DISCONNECT USING Sqlprod;
					lb_Conectado	=	False
				End If
			NEXT
		End If	
	End If	
	
	If ll_Filas = -1 Then
		F_ErrorBaseDatos(sqlca,"Recuperación datos de Inspección")
	ElseIf ll_Filas = 0 Then
		MessageBox("Atención", "No hay información para Inspección Indicada.~r~rIngrese otro Número.", Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_inspeccion.SetFocus()
	Else
		dw_2.SetSort('clie_codigo,paen_numero')
		dw_2.Sort()
	
		FOR ll_Fila = 1 TO ll_Filas
	
			li_PldSag	=	dw_2.Object.plde_codpla[ll_Fila]
			ls_Especie	=	dw_2.Object.espe_codsag[ll_Fila]
			
			If ls_Especie <> ls_EspeAnt Then
				ls_Registro	=	String(ll_Numero,'00000')
				ls_Registro +=	String(li_PldSag,'0000')
				ls_Registro	+=	ls_Especie
				ls_Registro	+=	String(dw_2.Object.dest_codigo[ll_Fila], '000')
				ls_Registro	+= '000'
				ls_Registro	+=	String(dw_2.Object.inpe_fechai[ll_Fila], 'YYYYMMDD')
				ls_Registro	+= String(dw_2.Object.contpallet[ll_Fila], '0000')
	
				ls_EspeAnt	=	ls_Especie
				
				ll_FilaDet	=	dw_1.InsertRow(0)
				dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
			End If
				
	//		ls_Registro =	String(Integer(istr_mant.argumento[1]),'000')
			ls_rotulacion = f_ClienteRotulado(dw_2.Object.clie_codigo[ll_fila])
			ls_Registro =	ls_rotulacion
			
			ls_Registro	+=	String(dw_2.Object.paen_numero[ll_Fila], '0000000')
			ls_Registro	+=	String(dw_2.Object.paen_ccajas[ll_Fila], '0000')
			
			ll_FilaDet	=	dw_1.InsertRow(0)
			dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
		NEXT
		ls_Registro	= '&&'
	
		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
		
		If li_region <>  4 Then
			ls_Archivo	= String(li_PldSag,'000') + String(ll_Numero,'00000') + ".INS"
		Else
			ls_Archivo	= String(li_PldSag,'0000') + String(ll_Numero,'00000') + ".INS"
		End If
	
		If dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 Then
			MessageBox("Atención","No se pudo generar el archivo "+ls_Archivo)
		Else
			sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
			
			ll_numinpe = Long(em_inspeccion.Text)
			
			UPDATE dbo.inspecpalenc 
				SET inpe_estado = 2
			WHERE inpe_numero = :ll_numinpe
			AND	:uo_SelCliente.Codigo in (-1,clie_codigo)
			AND	plde_codigo = :uo_SelPlanta.Codigo
			USING Sqlca;
			//This.TriggerEvent("ue_despuesguardar")
		End If
		dw_2.Reset()
	End If
	em_inspeccion.SetFocus()
Else
	MessageBox("Atención", "la Inspección Ingresada se encuentra Cerrada.~r~rIngrese otro Número.", Exclamation!, Ok!)
End If	

end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event type long ue_despuesguardar();SetPointer(HourGlass!)

Long		ll_Filas, ll_fila1, ll_numero, ll_nueva, ll_nueva1, ll_fila, ll_nueva2,&
			ll_fila2, ll_nueva3, ll_fila3, fila_find, ll_pallet, fila_find2, ll_numinpe
Boolean	lb_AutoCommit, lb_Retorno

IF em_inspeccion.Text = "" THEN 
	MessageBox( "Advertencia", "Falta Número de Inspección.", StopSign!, Ok!)
	RETURN 1
ELSE
	ll_numinpe = Long(em_inspeccion.Text)
END IF	

ll_Filas = dw_7.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_numinpe,1)
								
dw_8.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_numinpe,1)
								
IF ll_Filas = -1 THEN								
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	Return 1
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	Return 1
END IF	

IF uo_SelCliente.Codigo = -1 THEN uo_SelCliente.Inicia(gi_Cliebase)

Select max(altu_numero) + 1
into :ll_numero
from dbo.alpalletencab
where clie_codigo = :uo_SelCliente.Codigo
and	plde_codigo = :uo_SelPlanta.Codigo
and   altu_numero<99999999;

IF isnull(dw_7.Object.inpe_estado[1]) OR dw_7.Object.inpe_estado[1] = 0 THEN
	
	ll_nueva = dw_3.InsertRow(0)
	
	dw_3.Object.clie_codigo[ll_nueva] = uo_SelCliente.Codigo  
	dw_3.Object.plde_codigo[ll_nueva] = uo_SelPlanta.Codigo
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
			
			dw_4.Object.clie_codigo[ll_nueva1] = uo_SelCliente.Codigo  
			dw_4.Object.plde_codigo[ll_nueva1] = uo_SelPlanta.Codigo
			dw_4.Object.altu_numero[ll_nueva1] = ll_numero
			dw_4.Object.alpf_fecmov[ll_nueva1] = dw_8.Object.pafr_fecemb[ll_fila1]
			dw_4.Object.paen_numero[ll_nueva1] = dw_8.Object.paen_numero[ll_fila1]
		END IF			
	END IF
NEXT

FOR ll_fila2 = 1 TO dw_7.RowCount() 
	IF isnull(dw_7.Object.inpe_estado[1]) OR dw_7.Object.inpe_estado[1] = 0 THEN
		
		ll_nueva2 = dw_5.InsertRow(0)
		
		dw_5.Object.clie_codigo[ll_nueva2] = uo_SelCliente.Codigo   
		dw_5.Object.paen_numero[ll_nueva2] = dw_7.Object.paen_numero[ll_fila2]  
		dw_5.Object.plde_codigo[ll_nueva2] = uo_SelPlanta.Codigo  
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

		dw_6.Object.clie_codigo[ll_fila3] = uo_SelCliente.Codigo 
		dw_6.Object.paen_numero[ll_fila3] = dw_8.Object.paen_numero[ll_fila3]    
		dw_6.Object.espe_codigo[ll_fila3] = dw_8.Object.espe_codigo[ll_fila3]  
		dw_6.Object.vari_codigo[ll_fila3] = dw_8.Object.vari_codigo[ll_fila3]    
		dw_6.Object.emba_codigo[ll_fila3] = dw_8.Object.emba_codigo[ll_fila3]    
		dw_6.Object.prod_codigo[ll_fila3] = dw_8.Object.prod_codigo[ll_fila3]    
		dw_6.Object.cond_codigo[ll_fila3] = dw_8.Object.cond_codigo[ll_fila3]    
		dw_6.Object.etiq_codigo[ll_fila3] = dw_8.Object.etiq_codigo[ll_fila3]     
		dw_6.Object.plde_codigo[ll_fila3] = uo_SelPlanta.Codigo  
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
		
		dw_6.Object.pafr_huert4[ll_fila3] = dw_8.Object.pafr_huert4[ll_fila3]  
		dw_6.Object.pafr_cuart4[ll_fila3] = dw_8.Object.pafr_cuart4[ll_fila3] 
		
		dw_6.Object.pafr_calrot[ll_fila3] = dw_8.Object.pafr_calrot[ll_fila3] 
		dw_6.Object.pafr_rotpak[ll_fila3] = dw_8.Object.pafr_rotpak[ll_fila3] 
		dw_6.Object.pafr_prdrot[ll_fila3] = dw_8.Object.pafr_prdrot[ll_fila3] 
			
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
			Messagebox("Inspección","No se Pudo Grabar Registro en Tablas Históricas")
			Return 1
		END IF
		
		Messagebox("Inspección","Grabación de Datos Realizada Satisfactoriamente")
				
		Update dbo.inspecpalenc Set
		//inpe_estado = 1,
		inpe_proces = :ll_numero
		where inpe_numero = :ll_numinpe
		and	clie_codigo = :uo_SelCliente.Codigo
		and	plde_codigo = :uo_SelPlanta.Codigo
		Using Sqlca;
		
		sqlca.AutoCommit	=	lb_AutoCommit
		
		Return 1
END IF
								  

end event

public function boolean existeinspeccion (long al_inspec);Date		ld_fecha

IF al_inspec <> 0 OR uo_SelPlanta.Codigo = 0 THEN
	
	SELECT	Max(inpe_fechai),inpe_estado
		INTO	:ld_fecha, :ii_estado
		FROM	dbo.INSPECPALENC
		WHERE	inpe_numero	=	:al_inspec
		AND	inpe_tipoin =	1		
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	plde_codigo =	:uo_SelPlanta.Codigo
//		AND	inpe_secuen	=	1 
		group by inpe_estado;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla INSPECPALENC")
		em_inspeccion.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Nro. Inspección Indicado.~r~rIngrese otro Número.", Exclamation!, Ok!)
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
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_gene_archivo_inspeccion.create
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.cbx_conex=create cbx_conex
this.dw_9=create dw_9
this.dw_8=create dw_8
this.dw_7=create dw_7
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.cbx_var=create cbx_var
this.cbx_terceros=create cbx_terceros
this.st_7=create st_7
this.em_fecha=create em_fecha
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_inspeccion=create em_inspeccion
this.st_5=create st_5
this.st_2=create st_2
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_6=create st_6
this.st_1=create st_1
this.dw_2=create dw_2
this.dw_1=create dw_1
this.Control[]={this.uo_selplanta,&
this.uo_selcliente,&
this.cbx_conex,&
this.dw_9,&
this.dw_8,&
this.dw_7,&
this.dw_3,&
this.dw_4,&
this.dw_5,&
this.dw_6,&
this.cbx_var,&
this.cbx_terceros,&
this.st_7,&
this.em_fecha,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_inspeccion,&
this.st_5,&
this.st_2,&
this.pb_salir,&
this.pb_grabar,&
this.st_6,&
this.st_1,&
this.dw_2,&
this.dw_1}
end on

on w_gene_archivo_inspeccion.destroy
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.cbx_conex)
destroy(this.dw_9)
destroy(this.dw_8)
destroy(this.dw_7)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.cbx_var)
destroy(this.cbx_terceros)
destroy(this.st_7)
destroy(this.em_fecha)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_inspeccion)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_6)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.dw_1)
end on

event open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	x = 0
	y = 0
	
	This.Icon	=	Gstr_apl.Icono
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	IF gi_vari_rotulada = 1 THEN
		cbx_var.Checked	= True
		cbx_var.Enabled	=	False
	ELSE
		cbx_var.Checked	= False
		cbx_var.Enabled	=	True
	END IF	
	ii_var	= gi_vari_rotulada
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
								
	dw_7.SetTransObject(Sqlca)
	dw_8.SetTransObject(Sqlca)
	dw_3.SetTransObject(Sqlca)
	dw_4.SetTransObject(Sqlca)
	dw_5.SetTransObject(Sqlca)
	dw_6.SetTransObject(Sqlca)
End If
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_inspeccion
integer x = 690
integer y = 428
integer height = 92
integer taborder = 30
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_inspeccion
event destroy ( )
integer x = 690
integer y = 304
integer height = 92
integer taborder = 20
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_conex from checkbox within w_gene_archivo_inspeccion
integer x = 407
integer y = 832
integer width = 1253
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Incluye Otras Conexiones"
end type

type dw_9 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 709
integer y = 1688
integer width = 320
integer height = 192
integer taborder = 50
string title = "none"
string dataobject = "dw_prodconectivad_inscondi"
end type

type dw_8 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 398
integer y = 1244
integer width = 320
integer height = 192
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletfruta_inpe"
end type

type dw_7 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 745
integer y = 1252
integer width = 320
integer height = 192
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencab_inpe"
end type

type dw_3 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 41
integer y = 1448
integer width = 320
integer height = 192
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletencab"
end type

type dw_4 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 402
integer y = 1452
integer width = 320
integer height = 192
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta"
end type

type dw_5 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 731
integer y = 1484
integer width = 320
integer height = 192
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabhisto_inpe"
end type

type dw_6 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 357
integer y = 1660
integer width = 320
integer height = 192
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletfrutahisto_inpe"
end type

type cbx_var from checkbox within w_gene_archivo_inspeccion
boolean visible = false
integer x = 2213
integer y = 104
integer width = 146
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rot."
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type cbx_terceros from checkbox within w_gene_archivo_inspeccion
integer x = 407
integer y = 740
integer width = 1349
integer height = 116
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Terceros mismas Características"
end type

event clicked;IF This.Checked THEN
	uo_SelCliente.Bloquear(True)
	cbx_conex.Enabled = True
ELSE
	uo_SelCliente.Bloquear(False)
	cbx_conex.Enabled = False
	cbx_conex.Checked = False
END IF
end event

type st_7 from statictext within w_gene_archivo_inspeccion
integer x = 82
integer y = 720
integer width = 1911
integer height = 228
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_gene_archivo_inspeccion
integer x = 1253
integer y = 540
integer width = 430
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_4 from statictext within w_gene_archivo_inspeccion
integer x = 178
integer y = 440
integer width = 402
integer height = 64
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

type st_3 from statictext within w_gene_archivo_inspeccion
integer x = 178
integer y = 324
integer width = 402
integer height = 64
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

type sle_mensa from singlelineedit within w_gene_archivo_inspeccion
integer x = 128
integer y = 1004
integer width = 1829
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_inspeccion from editmask within w_gene_archivo_inspeccion
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 690
integer y = 540
integer width = 443
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string displaydata = "$"
end type

event modified;IF ExisteInspeccion(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type st_5 from statictext within w_gene_archivo_inspeccion
integer x = 82
integer y = 68
integer width = 1911
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Plano Inspección"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_archivo_inspeccion
integer x = 178
integer y = 556
integer width = 512
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nro. Inspección"
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_inspeccion
integer x = 2057
integer y = 920
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_inspeccion
integer x = 2057
integer y = 608
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type st_6 from statictext within w_gene_archivo_inspeccion
integer x = 82
integer y = 948
integer width = 1911
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_inspeccion
integer x = 82
integer y = 224
integer width = 1911
integer height = 496
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 64
integer y = 1248
integer width = 320
integer height = 192
string dataobject = "dw_gene_archivo_inspeccion_pro"
end type

event clicked;This.Print()
end event

event retrievestart;Return 2
end event

type dw_1 from datawindow within w_gene_archivo_inspeccion
boolean visible = false
integer x = 18
integer y = 1648
integer width = 320
integer height = 192
string dataobject = "dw_gene_archivo_saam_plano"
end type

