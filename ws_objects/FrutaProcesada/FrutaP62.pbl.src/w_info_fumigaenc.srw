$PBExportHeader$w_info_fumigaenc.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_info_fumigaenc from w_para_informes
end type
type st_4 from statictext within w_info_fumigaenc
end type
type st_1 from statictext within w_info_fumigaenc
end type
type st_5 from statictext within w_info_fumigaenc
end type
type st_2 from statictext within w_info_fumigaenc
end type
type em_numero from editmask within w_info_fumigaenc
end type
type dw_2 from datawindow within w_info_fumigaenc
end type
type st_6 from statictext within w_info_fumigaenc
end type
type dw_1 from datawindow within w_info_fumigaenc
end type
type cb_buscarepa from commandbutton within w_info_fumigaenc
end type
type st_3 from statictext within w_info_fumigaenc
end type
type dw_3 from datawindow within w_info_fumigaenc
end type
type cbx_varrot from checkbox within w_info_fumigaenc
end type
type st_7 from statictext within w_info_fumigaenc
end type
type dw_4 from datawindow within w_info_fumigaenc
end type
type dw_5 from datawindow within w_info_fumigaenc
end type
type dw_6 from datawindow within w_info_fumigaenc
end type
type dw_7 from datawindow within w_info_fumigaenc
end type
type dw_8 from datawindow within w_info_fumigaenc
end type
type dw_9 from datawindow within w_info_fumigaenc
end type
type rb_2 from radiobutton within w_info_fumigaenc
end type
type rb_1 from radiobutton within w_info_fumigaenc
end type
type cbx_prdrot from checkbox within w_info_fumigaenc
end type
type cbx_calrot from checkbox within w_info_fumigaenc
end type
type rb_3 from radiobutton within w_info_fumigaenc
end type
type cbx_terceros from checkbox within w_info_fumigaenc
end type
type cbx_packrot from checkbox within w_info_fumigaenc
end type
type rb_4 from radiobutton within w_info_fumigaenc
end type
type rb_5 from radiobutton within w_info_fumigaenc
end type
type cbx_mexicopredio from checkbox within w_info_fumigaenc
end type
type cbx_sdp from checkbox within w_info_fumigaenc
end type
type gb_4 from groupbox within w_info_fumigaenc
end type
type st_8 from statictext within w_info_fumigaenc
end type
type cbx_titulo from checkbox within w_info_fumigaenc
end type
type cbx_id from checkbox within w_info_fumigaenc
end type
type cbx_lote from checkbox within w_info_fumigaenc
end type
type st_10 from statictext within w_info_fumigaenc
end type
type cbx_nuevo from checkbox within w_info_fumigaenc
end type
type dw_10 from datawindow within w_info_fumigaenc
end type
type cbx_conex from checkbox within w_info_fumigaenc
end type
type rb_detalle from radiobutton within w_info_fumigaenc
end type
type rb_solicitud from radiobutton within w_info_fumigaenc
end type
type gb_3 from groupbox within w_info_fumigaenc
end type
type st_9 from statictext within w_info_fumigaenc
end type
type rb_planilla from radiobutton within w_info_fumigaenc
end type
type dw_11 from datawindow within w_info_fumigaenc
end type
end forward

global type w_info_fumigaenc from w_para_informes
integer x = 14
integer y = 32
integer width = 2693
integer height = 2328
string title = "INFORME DE CONDICIÓN"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event type integer ue_guardar ( )
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_numero em_numero
dw_2 dw_2
st_6 st_6
dw_1 dw_1
cb_buscarepa cb_buscarepa
st_3 st_3
dw_3 dw_3
cbx_varrot cbx_varrot
st_7 st_7
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
dw_7 dw_7
dw_8 dw_8
dw_9 dw_9
rb_2 rb_2
rb_1 rb_1
cbx_prdrot cbx_prdrot
cbx_calrot cbx_calrot
rb_3 rb_3
cbx_terceros cbx_terceros
cbx_packrot cbx_packrot
rb_4 rb_4
rb_5 rb_5
cbx_mexicopredio cbx_mexicopredio
cbx_sdp cbx_sdp
gb_4 gb_4
st_8 st_8
cbx_titulo cbx_titulo
cbx_id cbx_id
cbx_lote cbx_lote
st_10 st_10
cbx_nuevo cbx_nuevo
dw_10 dw_10
cbx_conex cbx_conex
rb_detalle rb_detalle
rb_solicitud rb_solicitud
gb_3 gb_3
st_9 st_9
rb_planilla rb_planilla
dw_11 dw_11
end type
global w_info_fumigaenc w_info_fumigaenc

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie, idwc_condicion

Long		il_sag
String	is_destino

uo_condicion    iuo_condicion     

Transaction	Sqlprod

w_informes2				vinf2

uo_odbc				iuo_odbc

end variables

forward prototypes
public function boolean noexistefolio (long al_numero)
end prototypes

event type integer ue_guardar();SetPointer(HourGlass!)

Long		ll_Filas, ll_fila1, ll_numero, ll_nueva, ll_nueva1, ll_fila, ll_nueva2,&
			ll_fila2, ll_nueva3, ll_fila3, fila_find, ll_pallet, fila_find2, ll_numerofumi
Integer	li_Cliente, li_planta,Respuesta
Boolean	lb_AutoCommit, lb_Retorno

IF em_numero.Text = "" THEN 
	MessageBox( "Advertencia", "Falta Número de Inspección.", &
					StopSign!, Ok!)
	RETURN 1
ELSE
	ll_numerofumi = Long(em_numero.Text)
END IF	

ll_Filas = dw_4.Retrieve(Integer(istr_mant.argumento[3]), &
								Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[9]),&
								ll_numerofumi)
								
dw_5.Retrieve(Integer(istr_mant.argumento[3]), &
								Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[9]),&
								ll_numerofumi)
								
IF ll_Filas = -1 THEN								
MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
					Return 1
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
					Return 1
END IF

IF isnull(dw_4.Object.fumi_proces[1]) OR dw_4.Object.fumi_proces[1] = 0 THEN
	
	Respuesta = MessageBox("Atención", "Desea grabar registros Históricos.",Exclamation!, OKCancel!, 2)
				
	IF Respuesta = 2 THEN
		Return 1
	END IF
	
	li_cliente = Integer(istr_mant.Argumento[3])
	li_planta =	 Integer(istr_mant.Argumento[1])
	
	SELECT max(altu_numero) + 1
	INTO :ll_numero
	FROM dbo.alpalletencab
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND   altu_numero < 99999999;
	
	ll_nueva = dw_8.InsertRow(0)
	
	dw_8.Object.clie_codigo[ll_nueva] = li_cliente  
	dw_8.Object.plde_codigo[ll_nueva] = li_planta
	dw_8.Object.altu_numero[ll_nueva] = ll_numero
	dw_8.Object.altu_fecmov[ll_nueva] = dw_4.Object.paen_fecemb[1]
	dw_8.Object.altu_observ[ll_nueva] = 'Fumigación'
	
	FOR ll_fila1 = 1 TO dw_5.RowCount() 
		
		ll_pallet = dw_5.Object.paen_numero[ll_fila1]
	
		fila_find2 =  dw_9.Find("paen_numero = " + String(ll_pallet),&
			1, dw_9.RowCount())
			
		IF fila_find2 = 0 THEN 
		
			ll_nueva1 = dw_9.InsertRow(0)
			
			dw_9.Object.clie_codigo[ll_nueva1] = li_cliente  
			dw_9.Object.plde_codigo[ll_nueva1] = li_planta
			dw_9.Object.altu_numero[ll_nueva1] = ll_numero
			dw_9.Object.alpf_fecmov[ll_nueva1] = dw_5.Object.pafr_fecemb[ll_fila1]
			dw_9.Object.paen_numero[ll_nueva1] = dw_5.Object.paen_numero[ll_fila1]
		END IF			
	
	NEXT
	
	FOR ll_fila2 = 1 TO dw_4.RowCount() 
		
		ll_nueva2 = dw_6.InsertRow(0)
		
		dw_6.Object.clie_codigo[ll_nueva2] = li_cliente   
		dw_6.Object.paen_numero[ll_nueva2] = dw_4.Object.paen_numero[ll_fila2]  
		dw_6.Object.plde_codigo[ll_nueva2] = li_planta  
		dw_6.Object.pahi_proces[ll_nueva2] = ll_numero  
		dw_6.Object.pahi_tipopa[ll_nueva2] = dw_4.Object.paen_tipopa[ll_fila2]  
		dw_6.Object.tpem_codigo[ll_nueva2] = dw_4.Object.tpem_codigo[ll_fila2]     
		dw_6.Object.espe_codigo[ll_nueva2] = dw_4.Object.espe_codigo[ll_fila2]    
		dw_6.Object.vari_codigo[ll_nueva2] = dw_4.Object.vari_codigo[ll_fila2]    
		dw_6.Object.tiem_codigo[ll_nueva2] = dw_4.Object.tiem_codigo[ll_fila2]    
		dw_6.Object.emba_codigo[ll_nueva2] = dw_4.Object.emba_codigo[ll_fila2]    
		dw_6.Object.cate_codigo[ll_nueva2] = dw_4.Object.cate_codigo[ll_fila2]    
		dw_6.Object.etiq_codigo[ll_nueva2] = dw_4.Object.etiq_codigo[ll_fila2]    
		dw_6.Object.stat_codigo[ll_nueva2] = dw_4.Object.stat_codigo[ll_fila2]    
		dw_6.Object.trat_codigo[ll_nueva2] = dw_4.Object.trat_codigo[ll_fila2]    
		dw_6.Object.frio_codigo[ll_nueva2] = dw_4.Object.frio_codigo[ll_fila2]    
		dw_6.Object.cond_codigo[ll_nueva2] = dw_4.Object.cond_codigo[ll_fila2]    
		dw_6.Object.dest_codigo[ll_nueva2] = dw_4.Object.dest_codigo[ll_fila2]    
		dw_6.Object.pahi_fecemb[ll_nueva2] = dw_4.Object.paen_fecemb[ll_fila2]    
		dw_6.Object.pahi_cosecha[ll_nueva2] = dw_4.Object.paen_cosecha[ll_fila2]     
		dw_6.Object.paen_altura[ll_nueva2] = dw_4.Object.paen_altura[ll_fila2]    
		dw_6.Object.paen_ccajas[ll_nueva2] = dw_4.Object.paen_ccajas[ll_fila2]    
		dw_6.Object.tmvp_codigo[ll_nueva2] = dw_4.Object.tmvp_codigo[ll_fila2]    
		dw_6.Object.paen_fecini[ll_nueva2] = dw_4.Object.paen_fecini[ll_fila2]    
		dw_6.Object.paen_horain[ll_nueva2] = dw_4.Object.paen_horain[ll_fila2]    
		dw_6.Object.cama_codigo[ll_nueva2] = dw_4.Object.cama_codigo[ll_fila2]    
		dw_6.Object.pahi_calle[ll_nueva2] = dw_4.Object.paen_calle[ll_fila2]    
		dw_6.Object.pahi_base[ll_nueva2] = dw_4.Object.paen_base[ll_fila2]   
		dw_6.Object.pahi_posici[ll_nueva2] = dw_4.Object.paen_posici[ll_fila2]    
		dw_6.Object.pahi_estado[ll_nueva2] = dw_4.Object.paen_estado[ll_fila2]    
		dw_6.Object.pahi_inspec[ll_nueva2] = dw_4.Object.paen_inspec[ll_fila2]    
		dw_6.Object.pahi_concal[ll_nueva2] = dw_4.Object.paen_concal[ll_fila2]    
		dw_6.Object.pahi_pexpor[ll_nueva2] = dw_4.Object.paen_pexpor[ll_fila2]
		dw_6.Object.pahi_pmixto[ll_nueva2] = dw_4.Object.paen_pmixto[ll_fila2]
	
	NEXT
	
	FOR ll_fila3 = 1 TO dw_5.RowCount() 
		
		ll_nueva3 = dw_6.InsertRow(0)
	
		dw_7.Object.clie_codigo[ll_fila3] = li_cliente 
		dw_7.Object.paen_numero[ll_fila3] = dw_5.Object.paen_numero[ll_fila3]    
		dw_7.Object.espe_codigo[ll_fila3] = dw_5.Object.espe_codigo[ll_fila3]  
		dw_7.Object.vari_codigo[ll_fila3] = dw_5.Object.vari_codigo[ll_fila3]    
		dw_7.Object.emba_codigo[ll_fila3] = dw_5.Object.emba_codigo[ll_fila3]    
		dw_7.Object.prod_codigo[ll_fila3] = dw_5.Object.prod_codigo[ll_fila3]    
		dw_7.Object.cond_codigo[ll_fila3] = dw_5.Object.cond_codigo[ll_fila3]    
		dw_7.Object.etiq_codigo[ll_fila3] = dw_5.Object.etiq_codigo[ll_fila3]     
		dw_7.Object.plde_codigo[ll_fila3] = li_planta  
		dw_7.Object.pafh_calibr[ll_fila3] = dw_5.Object.pafr_calibr[ll_fila3]    
		dw_7.Object.pafh_proces[ll_fila3] = ll_numero  
		dw_7.Object.pafh_secuen[ll_fila3] = dw_5.Object.pafr_secuen[ll_fila3]    
		dw_7.Object.pafh_ccajas[ll_fila3] = dw_5.Object.pafr_ccajas[ll_fila3]    
		dw_7.Object.pafh_nrlote[ll_fila3] = dw_5.Object.pafr_nrlote[ll_fila3]    
		dw_7.Object.pafr_fecing[ll_fila3] = dw_5.Object.pafr_fecing[ll_fila3]    
		dw_7.Object.pafr_fecemb[ll_fila3] = dw_5.Object.pafr_fecemb[ll_fila3]    
		dw_7.Object.pafr_copack[ll_fila3] = dw_5.Object.pafr_copack[ll_fila3]    
		dw_7.Object.pafr_tipdoc[ll_fila3] = 8 
		dw_7.Object.pafr_huert1[ll_fila3] = dw_5.Object.pafr_huert1[ll_fila3]  
		dw_7.Object.pafr_cuart1[ll_fila3] = dw_5.Object.pafr_cuart1[ll_fila3] 
		dw_7.Object.pafr_huert2[ll_fila3] = dw_5.Object.pafr_huert4[ll_fila3]  
		dw_7.Object.pafr_cuart2[ll_fila3] = dw_5.Object.pafr_cuart4[ll_fila3] 
		dw_7.Object.pafr_varrot[ll_fila3] = dw_5.Object.pafr_varrot[ll_fila3] 
	
	NEXT
	
	IF dw_8.Rowcount() > 0 THEN
		lb_AutoCommit		=	sqlca.AutoCommit
		sqlca.AutoCommit	=	False
		
		IF dw_6.Update(True, False) = 1 THEN
			IF dw_7.Update(True, False) = 1 THEN
				IF dw_8.Update(True, False) = 1 THEN
					IF dw_9.Update(True, False) = 1 THEN
						Commit;
						
						IF sqlca.SQLCode <> 0 THEN
					//		F_ErrorBaseDatos(sqlca, This.Title)
							
							RollBack;
							Return 1
						ELSE
							lb_Retorno	=	True
							
							dw_6.ResetUpdate()
							dw_7.ResetUpdate()
							dw_8.ResetUpdate()
							dw_9.ResetUpdate()
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
			Messagebox("Condición","No se Pudo grabar registro en tablas Históricas")
			Return 1
		END IF
		
		Messagebox("Condición","Grabación de Datos realizada Satisfactoriamente")
		
		UPDATE dbo.fumigaenc SET
		fumi_proces = :ll_numero
		WHERE fumi_numsag = :ll_numerofumi
		AND	clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		USING Sqlca;
	
		sqlca.AutoCommit	=	lb_AutoCommit
		
		Return 1
	END IF
	Return 1
ELSE	
	Return 1
END IF	

									  

end event

public function boolean noexistefolio (long al_numero);Integer	li_Cliente, li_Planta, li_count
Long 		ll_numero
Integer	li_mercado, li_destino

li_Cliente	=	Integer(istr_mant.argumento[3])
li_Planta	=	Integer(istr_mant.argumento[1])
ll_Numero	=	Long(em_numero.Text)

IF al_numero <> 0 THEN
	
	SELECT	max(merc_codigo)
		INTO	:li_mercado
		FROM	dbo.fumigaenc
		WHERE	fumi_numsag	=	:al_numero
		AND	clie_codigo	=	:li_cliente
		AND	plde_codigo =	:li_planta;
		
//	IF li_mercado = 6 THEN
//		rb_5.Enabled = True
//		rb_5.Checked = False
//	ELSE	
//		rb_5.Enabled = False
//	END IF
	
	SELECT	count(*)
		INTO	:li_count
		FROM	dbo.fumigaenc
		WHERE	fumi_numsag	=	:al_numero
		AND	clie_codigo	=	:li_cliente
		AND	plde_codigo =	:li_planta;
			
//	SELECT	fumi_numsag,merc_codigo
//		INTO	:il_sag,:li_destino
//		FROM	dbo.fumigaenc
//		WHERE	fumi_numero	=	:al_numero
//		AND	clie_codigo	=	:li_cliente
//		AND	plde_codigo =	:li_planta;	
//		
//	SELECT merc_nombre
//		INTO :is_destino
//		FROM dbo.mercado
//		WHERE merc_codigo = :li_destino;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Fumigaenc")
		em_numero.SetFocus()
		RETURN False
	ELSEIF li_count = 0 THEN
		MessageBox("Atención", "No Existe Nro.Condición Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()
		em_numero.Text = '' 
		RETURN False
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF

RETURN False

end function

on w_info_fumigaenc.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.cb_buscarepa=create cb_buscarepa
this.st_3=create st_3
this.dw_3=create dw_3
this.cbx_varrot=create cbx_varrot
this.st_7=create st_7
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_9=create dw_9
this.rb_2=create rb_2
this.rb_1=create rb_1
this.cbx_prdrot=create cbx_prdrot
this.cbx_calrot=create cbx_calrot
this.rb_3=create rb_3
this.cbx_terceros=create cbx_terceros
this.cbx_packrot=create cbx_packrot
this.rb_4=create rb_4
this.rb_5=create rb_5
this.cbx_mexicopredio=create cbx_mexicopredio
this.cbx_sdp=create cbx_sdp
this.gb_4=create gb_4
this.st_8=create st_8
this.cbx_titulo=create cbx_titulo
this.cbx_id=create cbx_id
this.cbx_lote=create cbx_lote
this.st_10=create st_10
this.cbx_nuevo=create cbx_nuevo
this.dw_10=create dw_10
this.cbx_conex=create cbx_conex
this.rb_detalle=create rb_detalle
this.rb_solicitud=create rb_solicitud
this.gb_3=create gb_3
this.st_9=create st_9
this.rb_planilla=create rb_planilla
this.dw_11=create dw_11
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_1
this.Control[iCurrent+9]=this.cb_buscarepa
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.dw_3
this.Control[iCurrent+12]=this.cbx_varrot
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.dw_4
this.Control[iCurrent+15]=this.dw_5
this.Control[iCurrent+16]=this.dw_6
this.Control[iCurrent+17]=this.dw_7
this.Control[iCurrent+18]=this.dw_8
this.Control[iCurrent+19]=this.dw_9
this.Control[iCurrent+20]=this.rb_2
this.Control[iCurrent+21]=this.rb_1
this.Control[iCurrent+22]=this.cbx_prdrot
this.Control[iCurrent+23]=this.cbx_calrot
this.Control[iCurrent+24]=this.rb_3
this.Control[iCurrent+25]=this.cbx_terceros
this.Control[iCurrent+26]=this.cbx_packrot
this.Control[iCurrent+27]=this.rb_4
this.Control[iCurrent+28]=this.rb_5
this.Control[iCurrent+29]=this.cbx_mexicopredio
this.Control[iCurrent+30]=this.cbx_sdp
this.Control[iCurrent+31]=this.gb_4
this.Control[iCurrent+32]=this.st_8
this.Control[iCurrent+33]=this.cbx_titulo
this.Control[iCurrent+34]=this.cbx_id
this.Control[iCurrent+35]=this.cbx_lote
this.Control[iCurrent+36]=this.st_10
this.Control[iCurrent+37]=this.cbx_nuevo
this.Control[iCurrent+38]=this.dw_10
this.Control[iCurrent+39]=this.cbx_conex
this.Control[iCurrent+40]=this.rb_detalle
this.Control[iCurrent+41]=this.rb_solicitud
this.Control[iCurrent+42]=this.gb_3
this.Control[iCurrent+43]=this.st_9
this.Control[iCurrent+44]=this.rb_planilla
this.Control[iCurrent+45]=this.dw_11
end on

on w_info_fumigaenc.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cb_buscarepa)
destroy(this.st_3)
destroy(this.dw_3)
destroy(this.cbx_varrot)
destroy(this.st_7)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_9)
destroy(this.rb_2)
destroy(this.rb_1)
destroy(this.cbx_prdrot)
destroy(this.cbx_calrot)
destroy(this.rb_3)
destroy(this.cbx_terceros)
destroy(this.cbx_packrot)
destroy(this.rb_4)
destroy(this.rb_5)
destroy(this.cbx_mexicopredio)
destroy(this.cbx_sdp)
destroy(this.gb_4)
destroy(this.st_8)
destroy(this.cbx_titulo)
destroy(this.cbx_id)
destroy(this.cbx_lote)
destroy(this.st_10)
destroy(this.cbx_nuevo)
destroy(this.dw_10)
destroy(this.cbx_conex)
destroy(this.rb_detalle)
destroy(this.rb_solicitud)
destroy(this.gb_3)
destroy(this.st_9)
destroy(this.rb_planilla)
destroy(this.dw_11)
end on

event open;call super::open;String	ls_Planta

SELECT	plde_nombre
	INTO	:ls_Planta
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve(-1)
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(-1)
dw_1.InsertRow(0)
dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_3.GetChild("cond_codigo", idwc_condicion)
idwc_condicion.SetTransObject(sqlca)
idwc_condicion.Retrieve(-1)
dw_3.InsertRow(0)
dw_3.SetItem(1, "cond_codigo", 1)

IF gi_vari_rotulada = 1 THEN
	cbx_varrot.Checked	= True
	cbx_varrot.Enabled	= False
ELSE
	cbx_varrot.Checked	= False
	cbx_varrot.Enabled	= True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_prdrot.Checked	= True
	cbx_prdrot.Enabled	= False
ELSE
	cbx_prdrot.Checked	= False
	cbx_prdrot.Enabled	= True
END IF	

IF gi_cali_rotulado = 1 THEN
	cbx_calrot.Checked	= True
	cbx_calrot.Enabled	= False
ELSE
	cbx_calrot.Checked	= False
	cbx_calrot.Enabled	= True
END IF	

IF gi_pack_rotulado = 1 THEN
	cbx_packrot.Checked	= True
	cbx_packrot.Enabled	= False
ELSE
	cbx_packrot.Checked	= False
	cbx_packrot.Enabled	= True
END IF	

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[2]	=	""//folio
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[5]	=	ls_Planta
istr_mant.argumento[9]  =  '1' //condición

dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_8.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_10.SetTransObject(sqlca)




end event

type pb_excel from w_para_informes`pb_excel within w_info_fumigaenc
integer x = 2299
integer y = 996
end type

type st_computador from w_para_informes`st_computador within w_info_fumigaenc
end type

type st_usuario from w_para_informes`st_usuario within w_info_fumigaenc
end type

type st_temporada from w_para_informes`st_temporada within w_info_fumigaenc
end type

type p_logo from w_para_informes`p_logo within w_info_fumigaenc
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_fumigaenc
integer width = 1902
string text = "Informe de Condición"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_fumigaenc
string tag = "Imprimir Reporte"
integer x = 2322
integer y = 1540
integer taborder = 60
alignment htextalign = center!
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero,li_varrot, Respuesta , li_prdrot, li_calrot, li_cliente2,&
			li_packrot, li_marca, li_lote, ll_fila, li_info,li_cont, ll_cont, ll_fil, ll_Filas, li_conexion
Long		li_planta,li_cliente,ll_cantidad,ll_pallet, ll_tramoa, ll_tramob, ll_tramoc
String	ls_mensaje,  ls_colu[],ls_conexion,ls_nomodb,ls_nomser,ls_nombas,li_base,ls_Usuario,ls_Password,&
			ls_ubicacion,ls_ip,ls_puerto,ls_nodbms,ls_Motor
Boolean	lb_Conectado
Dec		ld_peso, ld_kiloa, ld_kilob, ld_kiloc

istr_info.titulo	= 'FUMIGACION DE PALLET'

OpenWithParm(vinf2, istr_info)

IF cbx_varrot.Checked THEN
	li_varrot 	=	1
ELSE
	li_varrot	=	0
END IF

IF cbx_prdrot.Checked THEN
	li_prdrot 	=	1
ELSE
	li_prdrot	=	0
END IF

IF cbx_calrot.Checked THEN
	li_calrot 	=	1
ELSE
	li_calrot	=	0
END IF

IF cbx_packrot.Checked THEN
	li_packrot 	=	1
ELSE
	li_packrot	=	0
END IF

IF cbx_id.Checked THEN
	li_marca = 0
ELSE
	li_marca = 1
END IF	

IF em_numero.text ="" THEN
	istr_mant.argumento[2] = '-1'
END IF

IF cbx_lote.Checked THEN
	li_lote = 1
ELSE
	li_lote = 0
END IF

IF cbx_conex.Checked = False THEN
	IF cbx_nuevo.Checked THEN
		// nvarchar
		vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_formato_mexico_nuevo"
	ELSE	
		IF rb_2.Checked THEN
			//MessageBox( 'Message', 'Informe 2')
			vinf2.dw_1.DataObject = "dw_info_fumigado_enc"
		ELSEIF rb_1.Checked THEN
			IF cbx_titulo.Checked THEN
				// dbo.fproc_informeresumenfumigacion_condicion 
				vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_condicion"
			ELSE
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_encsag"
			END IF	
		ELSEif rb_3.Checked THEN
			 IF cbx_sdp.Checked THEN
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_2SDP"
			 ELSE
				
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_2"
			 END IF
		ELSEif rb_5.Checked THEN
			IF cbx_mexicopredio.Checked THEN
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_formato_mexico"	
			ELSE
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_formato_mexico2"
			END IF
		ELSE
			// LISTO
			vinf2.dw_1.DataObject = "dw_info_fumigado_encsag_2_usda_insp"
			//dw_info_fumigado_sagsag_2_usda_insp_ori detalle original
		END IF
	END IF
	
	vinf2.dw_1.SetTransObject(sqlca)
	IF cbx_nuevo.Checked THEN
		ll_Fila = dw_10.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),&
											Integer(istr_mant.argumento[2]),Long(istr_mant.argumento[9]),&
											li_varrot,li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
		
		IF ll_Fila > 0 THEN
			
			FOR ll_cont = 1 TO dw_10.RowCount() 
				IF Isnull(dw_10.Object.prpr_prepro[ll_cont]) OR dw_10.Object.prpr_prepro[ll_cont] = '' THEN
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta CSG para Predio del Productor "+ String(dw_10.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "prpr_prepro"
				END IF
				
				IF Isnull(dw_10.Object.plde_codsag[ll_cont]) OR dw_10.Object.plde_codsag[ll_cont] = 0 THEN
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta Código Sag (CSP)para el Packing "+ String(dw_10.Object.pafr_copack[ll_cont])+' y Productor '+ String(dw_10.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "plde_codsag"
				END IF
			NEXT
			IF li_cont > 0 THEN
					MessageBox("Error de Consistencia", "Faltan Datos en Tablas :" + ls_mensaje + ".", StopSign!, Ok!)
		//			dw_1.SetColumn(ls_colu[1])
		//			dw_1.SetFocus()
		//			Message.DoubleParm = -1
				END IF
		END IF	
	END IF
	fila	=	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Long(istr_mant.Argumento[2]), &
						Integer(istr_mant.Argumento[3]),Integer(istr_mant.argumento[9]),li_varrot,&
						li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
ELSE						
	IF cbx_nuevo.Checked THEN
		IF rb_detalle.Checked THEN
			// LISTO
			vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_formato_mexico_nuevo"
		ELSEIF rb_solicitud.Checked THEN	
			// LISTO
			vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag_nuevo_conex"
		ELSE	
			// LISTO
			vinf2.dw_1.DataObject = "dw_info_fumigacion_fito_detalle_nuevo"
		END IF
			
	ELSE	
		IF rb_2.Checked THEN
			IF rb_detalle.Checked THEN
					// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_sag"
			ELSEIF rb_solicitud.Checked THEN	
					// LISTO
				vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detalle"
			ELSE
				MessageBox("Atención", "Ese Informe no está disponible para esa opción.", &
					Exclamation!, Ok!)
				RETURN 
			END IF
						
		ELSEIF rb_1.Checked THEN
			IF cbx_titulo.Checked THEN
				IF rb_detalle.Checked THEN
						// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_condicion"
				ELSEIF rb_solicitud.Checked THEN	
						// LISTO
					vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag_condicion"
				ELSE
					MessageBox("Atención", "Ese Informe no está disponible para esa opción.", &
						Exclamation!, Ok!)
					RETURN 	
				END IF
				
			ELSE
				IF rb_detalle.Checked THEN
						// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag"
				ELSEIF rb_solicitud.Checked THEN	
						// LISTO
					vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag"
				ELSE
					MessageBox("Atención", "Ese Informe no está disponible para esa opción.", &
						Exclamation!, Ok!)
					RETURN 	
				END IF
			END IF	
			
		ELSEif rb_3.Checked THEN
			 IF cbx_sdp.Checked THEN
				IF rb_detalle.Checked THEN
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_2sdp"
				ELSEIF rb_solicitud.Checked THEN	 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag"
				ELSE 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigacion_fito_detalle"	
				END IF
				
			 ELSE
				IF rb_detalle.Checked THEN
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_2"
				ELSEIF rb_solicitud.Checked THEN	 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag"
				ELSE 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigacion_fito_detalle"	
				END IF
			 END IF
			 
		ELSEif rb_5.Checked THEN
			IF cbx_mexicopredio.Checked THEN
				IF rb_detalle.Checked THEN
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_formato_mexico"
				ELSEIF rb_solicitud.Checked THEN	 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag"
				ELSE 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigacion_fito_detalle"	
				END IF
				
			ELSE
				IF rb_detalle.Checked THEN
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_formato_mexico2"
				ELSEIF rb_solicitud.Checked THEN	 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag"
				ELSE 
					// LISTO
					vinf2.dw_1.DataObject = "dw_info_fumigacion_fito_detalle"	
				END IF
			END IF
			
		ELSE
			IF rb_detalle.Checked THEN
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigado_sagsag_2_usda_insp"
			ELSEIF rb_solicitud.Checked THEN	 
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_resumen_fumiga_detallesag"
			ELSE 
				// LISTO
				vinf2.dw_1.DataObject = "dw_info_fumigacion_fito_detalle"	
			END IF
		END IF
	END IF
	
	vinf2.dw_1.SetTransObject(sqlca)
	IF cbx_nuevo.Checked THEN
		ll_Fila = dw_10.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),&
											Integer(istr_mant.argumento[2]),Long(istr_mant.argumento[9]),&
											li_varrot,li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
		
		IF ll_Fila > 0 THEN
			
			FOR ll_cont = 1 TO dw_10.RowCount() 
				IF Isnull(dw_10.Object.prpr_prepro[ll_cont]) OR dw_10.Object.prpr_prepro[ll_cont] = '' THEN
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta CSG para Predio del Productor "+ String(dw_10.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "prpr_prepro"
				END IF
				
				IF Isnull(dw_10.Object.plde_codsag[ll_cont]) OR dw_10.Object.plde_codsag[ll_cont] = 0 THEN
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta Código Sag (CSP)para el Packing "+ String(dw_10.Object.pafr_copack[ll_cont])+' y Productor '+ String(dw_10.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "plde_codsag"
				END IF
			NEXT
			IF li_cont > 0 THEN
					MessageBox("Error de Consistencia", "Faltan Datos en Tablas :" + ls_mensaje + ".", StopSign!, Ok!)
			END IF
		END IF	
	END IF
	
	
//	fila	=	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Long(istr_mant.Argumento[2]), &
//						Integer(istr_mant.Argumento[3]),Integer(istr_mant.argumento[9]),li_varrot,&
//						li_prdrot,li_calrot,li_packrot,li_marca,li_lote)

	IF rb_detalle.Checked THEN
		Fila =	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[3]),Long(istr_mant.Argumento[1]), &
			Integer(istr_mant.Argumento[2]),Integer(istr_mant.argumento[9]),li_varrot,&
			li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
	ELSEIF rb_solicitud.Checked THEN
		Fila =	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Long(istr_mant.Argumento[2]), &
			Integer(istr_mant.Argumento[3]),Integer(istr_mant.argumento[9]),li_varrot,&
			li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
	ELSE		
		Fila =	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[3]),Long(istr_mant.Argumento[1]), &
			Integer(istr_mant.Argumento[2]),Integer(istr_mant.argumento[9]),li_varrot,&
			li_prdrot,li_calrot)
	END IF	
	
END IF		

dw_11.SetTransObject(SqlCa)

IF cbx_conex.Checked THEN
	ll_fil = dw_11.Retrieve(1,-1) 
	IF ll_fil > 0 THEN
			
		Sqlprod	=	Create Transaction
		
		FOR ll_fila =  1 TO dw_2.RowCount() 
				
			li_conexion	=	dw_11.Object.cone_codigo[ll_fila]
			ls_conexion	=	dw_11.Object.cone_descri[ll_fila]
			ls_nomodb	=	dw_11.Object.cone_nomodb[ll_fila]
			ls_nomser	=	dw_11.Object.cone_nomser[ll_fila]
			ls_nombas   =  dw_11.Object.cone_nombas[ll_fila]
			ls_nodbms	=	dw_11.Object.cone_nodbms[ll_fila]
			ls_Usuario	=	dw_11.Object.cone_nomusu[ll_fila]
			ls_Password	=	dw_11.Object.cone_passwo[ll_fila]	
			ls_ubicacion	=	dw_11.Object.cone_ubicac[ll_fila]
			ls_ip			=	dw_11.Object.cone_ipserv[ll_fila]
			ls_puerto	=	dw_11.Object.cone_puerto[ll_fila]	
					
			IF lb_Conectado THEN 
				DISCONNECT USING Sqlprod;
				lb_Conectado	=	False
			END IF
			
			iuo_odbc			=	Create uo_odbc
					
			ls_Motor = iuo_Odbc.MotorBD()
			
			IF ls_Motor = '' OR ls_ubicacion = '' OR ls_ip = '' OR ls_puerto = '' THEN
				MessageBox("Atención", "Falta mantención a tabla conexiones.")
				lb_Conectado = False
			END IF	
			
			iuo_Odbc.Crea(ls_nomodb, ls_Usuario, ls_Password,&
				 ls_nombas,ls_ubicacion,ls_nomser,ls_ip, &
				 ls_puerto, ls_motor)
		
			Sqlprod.ServerName	=	ls_nomser
			Sqlprod.DataBase		=	ls_nombas
			Sqlprod.Dbms			= 	ls_nodbms
			Sqlprod.DbParm			=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
											";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
		
			CONNECT USING Sqlprod;
			
			IF Sqlprod.SQLCode = 0 THEN 
				lb_Conectado = True
														
				vinf2.dw_1.SetTransObject(Sqlprod)
								
				IF rb_detalle.Checked THEN
					Fila =	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[3]),Long(istr_mant.Argumento[1]), &
							Integer(istr_mant.Argumento[2]),Integer(istr_mant.argumento[9]),li_varrot,&
							li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
				ELSEIF rb_solicitud.Checked THEN
					Fila =	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Long(istr_mant.Argumento[2]), &
						Integer(istr_mant.Argumento[3]),Integer(istr_mant.argumento[9]),li_varrot,&
						li_prdrot,li_calrot,li_packrot,li_marca,li_lote)
				ELSE		
					Fila =	vinf2.dw_1.Retrieve(Integer(istr_mant.Argumento[3]),Long(istr_mant.Argumento[1]), &
						Integer(istr_mant.Argumento[2]),Integer(istr_mant.argumento[9]),li_varrot,&
						li_prdrot,li_calrot)
				END IF	
			END IF	
			
			IF lb_Conectado THEN 
				DISCONNECT USING Sqlprod;
				lb_Conectado	=	False
			END IF
		NEXT
	END IF	
END IF	

IF cbx_conex.Checked THEN
	IF rb_solicitud.Checked THEN
		ld_peso 		= 0
		ll_cantidad = 0
		ll_pallet 	= 0
		ld_kiloa		= 0
		ld_kilob		= 0
		ld_kiloc		= 0
		
		FOR ll_fila = 1 TO vinf2.dw_1.RowCount()
			li_cliente = vinf2.dw_1.Object.clie_codigo[ll_fila]
			IF li_cliente2 <> li_cliente THEN
				ll_cantidad = ll_cantidad + vinf2.dw_1.Object.compute_3[ll_fila]
				ld_peso 		= ld_peso + vinf2.dw_1.Object.compute_2[ll_fila]
								
				ld_kiloa = ld_kiloa + vinf2.dw_1.Object.kilosa[ll_fila]
				ld_kilob = ld_kilob + vinf2.dw_1.Object.kilosb[ll_fila]
				ld_kiloc = ld_kiloc + vinf2.dw_1.Object.kilosc[ll_fila]
				
				ll_tramoa = ll_tramoa + vinf2.dw_1.Object.tramoa[ll_fila]
				ll_tramob = ll_tramob + vinf2.dw_1.Object.tramob[ll_fila]
				ll_tramoc = ll_tramoc + vinf2.dw_1.Object.tramoc[ll_fila]
							
				li_cliente2 = li_cliente
			END IF	
		NEXT	
	END IF
END IF	

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
				StopSign!, Ok!)
	ELSE
		dw_4.Reset()
		dw_5.Reset()
		dw_6.Reset()
		dw_7.Reset()
		dw_8.Reset()
		dw_9.Reset()
		
		dw_4.SetTransObject(sqlca)
		dw_5.SetTransObject(sqlca)
		dw_6.SetTransObject(sqlca)
		dw_7.SetTransObject(sqlca)
		dw_8.SetTransObject(sqlca)
		dw_9.SetTransObject(sqlca)
		
		IF cbx_conex.Checked <> True THEN
			w_info_fumigaenc.TriggerEvent("ue_guardar")
		END IF	
		
		IF gs_Ambiente = 'Windows' THEN
			vinf2.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
			vinf2.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 100')
			
			IF ll_cantidad > 0 THEN
				vinf2.dw_1.Modify("t_lotes.text = '" + String(ll_cantidad,'#,###') + "'")
				vinf2.dw_1.Modify("t_kilos.text = '" + String(ld_peso,'#,###.00') + "'")
								
				vinf2.dw_1.Modify("t_kilosa.text = '" + String(ld_kiloa,'#,###.00') + "'")
				vinf2.dw_1.Modify("t_kilosb.text = '" + String(ld_kilob,'#,###.00') + "'")
				vinf2.dw_1.Modify("t_kilosc.text = '" + String(ld_kiloc,'#,###.00') + "'")
				
				vinf2.dw_1.Modify("t_tramoa.text = '" + String(ll_tramoa,'#,###') + "'")
				vinf2.dw_1.Modify("t_tramob.text = '" + String(ll_tramob,'#,###') + "'")
				vinf2.dw_1.Modify("t_tramoc.text = '" + String(ll_tramoc,'#,###') + "'")
			END IF	
			
			IF vinf2.dw_1.Object.DataWindow.Print.Orientation = '1' THEN
				vinf2.dw_1.width = 5100//LandScape
			ELSE
				vinf2.dw_1.width = 3900//Portrait
			END IF
			
			IF vinf2.dw_1.RowCount() > 0 THEN
				vinf2.Visible	= 	True
				vinf2.Enabled	= 	True
			END IF
		
		END IF
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf2.dw_1, istr_info.titulo)
		END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_fumigaenc
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2318
integer y = 1844
integer taborder = 80
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_fumigaenc
integer x = 251
integer y = 420
integer width = 1902
integer height = 340
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_fumigaenc
integer x = 343
integer y = 560
integer width = 462
integer height = 76
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_fumigaenc
integer x = 251
integer y = 764
integer width = 1902
integer height = 124
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_fumigaenc
integer x = 352
integer y = 796
integer width = 517
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
string text = "Nro Folio SAG"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_fumigaenc
integer x = 891
integer y = 776
integer width = 393
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF This.Text <> "" THEN
	IF NoExisteFolio(Long(This.Text)) THEN
		This.Text	=	""
		This.SetFocus()
	ELSE
		istr_mant.argumento[2]	=	String(Long(This.Text), '00000000')
		
		dw_4.Reset()
		dw_5.Reset()
		dw_6.Reset()
		dw_7.Reset()
		dw_8.Reset()
		dw_9.Reset()
		
		dw_4.SetTransObject(sqlca)
		dw_5.SetTransObject(sqlca)
		dw_6.SetTransObject(sqlca)
		dw_7.SetTransObject(sqlca)
		dw_8.SetTransObject(sqlca)
		dw_9.SetTransObject(sqlca)
	END IF
END IF




end event

type dw_2 from datawindow within w_info_fumigaenc
integer x = 896
integer y = 452
integer width = 1161
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_planta	

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_fumigaenc
integer x = 343
integer y = 460
integer width = 233
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

type dw_1 from datawindow within w_info_fumigaenc
integer x = 901
integer y = 552
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExistePlanta(Integer(istr_mant.argumento[3]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.Argumento[5]	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cb_buscarepa from commandbutton within w_info_fumigaenc
integer x = 1339
integer y = 780
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	istr_mant.argumento[3]
istr_busq.argum[2]	=	istr_mant.argumento[1]
istr_busq.argum[3]	=	String(dw_3.Object.cond_codigo[1])
istr_busq.argum[10] = ''

OpenWithParm(w_busc_fumigaenc, istr_busq)

dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()

dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_8.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)

istr_busq	       = Message.PowerObjectParm

em_numero.SetFocus()
IF istr_busq.argum[10] <> "" THEN
	
	IF NoExisteFolio(Long(istr_busq.argum[10])) THEN
		em_numero.Text	=	""
		em_numero.SetFocus()
	END IF	
	
	em_numero.Text			= istr_busq.argum[10]
	istr_mant.argumento[2]	= istr_busq.argum[10]
ELSE
	em_numero.SetFocus()
END IF
end event

type st_3 from statictext within w_info_fumigaenc
integer x = 343
integer y = 656
integer width = 462
integer height = 76
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
string text = "Condición"
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_info_fumigaenc
integer x = 896
integer y = 644
integer width = 960
integer height = 96
integer taborder = 40
boolean bringtotop = true
string dataobject = "dddw_condicion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null

SetNull(li_Null)
iuo_condicion  = Create uo_condicion

IF iuo_condicion.Existe(Integer(Data),True,SqlCa) THEN
   istr_mant.argumento[9] = Data
	IF Data = '2' THEN
		rb_4.Enabled = True
	ELSE
		rb_4.Enabled = False
	END IF	
ELSE
	This.SetItem(1,"cond_codigo",li_Null)
	rb_4.Enabled = False
	RETURN 1
END IF


end event

event itemerror;RETURN 1
end event

type cbx_varrot from checkbox within w_info_fumigaenc
integer x = 347
integer y = 908
integer width = 695
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
boolean checked = true
end type

type st_7 from statictext within w_info_fumigaenc
integer x = 251
integer y = 892
integer width = 1902
integer height = 192
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_4 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 3031
integer y = 288
integer width = 210
integer height = 180
integer taborder = 100
boolean bringtotop = true
string title = "fumigaenc"
string dataobject = "dw_fumigaenc_historia"
end type

type dw_5 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2249
integer y = 608
integer width = 133
integer height = 124
integer taborder = 130
boolean bringtotop = true
string title = "fumigadet"
string dataobject = "dw_fumigadet_historia"
boolean minbox = true
boolean maxbox = true
boolean livescroll = true
end type

type dw_6 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2976
integer y = 492
integer width = 338
integer height = 168
integer taborder = 70
boolean bringtotop = true
string title = "pallet encab histo"
string dataobject = "dw_palletencabhisto_inpe"
end type

type dw_7 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2450
integer y = 448
integer width = 165
integer height = 148
integer taborder = 20
boolean bringtotop = true
string title = "palletfruta histo"
string dataobject = "dw_palletfrutahisto_inpe"
end type

type dw_8 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2903
integer y = 736
integer width = 320
integer height = 212
integer taborder = 90
boolean bringtotop = true
string title = "alpalletencab"
string dataobject = "dw_mues_alpalletencab"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
boolean livescroll = true
end type

type dw_9 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2240
integer y = 456
integer width = 155
integer height = 112
integer taborder = 110
boolean bringtotop = true
string title = "alpalletfruta"
string dataobject = "dw_mues_alpalletfruta"
boolean resizable = true
end type

type rb_2 from radiobutton within w_info_fumigaenc
integer x = 443
integer y = 1464
integer width = 635
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
string text = "Formato Genérico"
end type

event clicked;cbx_sdp.Enabled	=	False

cbx_titulo.Checked = False
cbx_titulo.Enabled = False
rb_planilla.Enabled = False
rb_planilla.Checked = False
end event

type rb_1 from radiobutton within w_info_fumigaenc
integer x = 443
integer y = 1144
integer width = 759
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
string text = "Formato Único S.A.G."
boolean checked = true
end type

event clicked;cbx_sdp.Enabled	=	False

IF This.Checked THEN
	cbx_titulo.Enabled = True
ELSE
	cbx_titulo.Enabled = False
	cbx_titulo.Checked = False
END IF	
end event

type cbx_prdrot from checkbox within w_info_fumigaenc
integer x = 1129
integer y = 908
integer width = 850
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rotulado Repa  "
end type

type cbx_calrot from checkbox within w_info_fumigaenc
integer x = 1129
integer y = 984
integer width = 850
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calibre Rotulado Repa  "
end type

type rb_3 from radiobutton within w_info_fumigaenc
integer x = 443
integer y = 1224
integer width = 855
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
string text = "Formato Incluye Packing"
end type

event clicked;IF This.Checked THEN
	cbx_sdp.Enabled	=	True
	cbx_titulo.Checked = False
	cbx_titulo.Enabled = False

ELSE
	cbx_sdp.Enabled	=	False
END IF
end event

type cbx_terceros from checkbox within w_info_fumigaenc
integer x = 562
integer y = 1680
integer width = 1317
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Terceros Mismas Características"
end type

event clicked;IF This.Checked THEN
	dw_2.Enabled		=	False
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	cbx_conex.Enabled = True
	cbx_conex.Checked = False
	istr_mant.argumento[3]	=	'-1'
ELSE
	dw_2.Enabled		=	True
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[3]	=	String(dw_2.Object.clie_codigo[1])
	cbx_conex.Enabled = False
	cbx_conex.Checked = False
	rb_detalle.Enabled = False
	rb_detalle.Checked = False
	rb_solicitud.Enabled = False
	rb_solicitud.Checked = False
	rb_planilla.Enabled = False
	rb_planilla.Checked = False
END IF

end event

type cbx_packrot from checkbox within w_info_fumigaenc
integer x = 347
integer y = 984
integer width = 695
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing Rotulado"
end type

type rb_4 from radiobutton within w_info_fumigaenc
integer x = 443
integer y = 1304
integer width = 951
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
boolean enabled = false
string text = "USDA INSPECTION"
end type

event clicked;cbx_sdp.Enabled	=	False
cbx_titulo.Checked = False
cbx_titulo.Enabled = False
end event

type rb_5 from radiobutton within w_info_fumigaenc
integer x = 443
integer y = 1384
integer width = 686
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
string text = "FORMATO MEXICO"
end type

event clicked;cbx_sdp.Enabled	=	False

cbx_titulo.Checked = False
cbx_titulo.Enabled = False


end event

type cbx_mexicopredio from checkbox within w_info_fumigaenc
integer x = 1298
integer y = 1360
integer width = 558
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Predios"
end type

type cbx_sdp from checkbox within w_info_fumigaenc
integer x = 1298
integer y = 1224
integer width = 594
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
boolean enabled = false
string text = "Códigos SDP"
end type

type gb_4 from groupbox within w_info_fumigaenc
integer x = 311
integer y = 1096
integer width = 1769
integer height = 452
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_8 from statictext within w_info_fumigaenc
integer x = 251
integer y = 1088
integer width = 1902
integer height = 492
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_titulo from checkbox within w_info_fumigaenc
integer x = 1298
integer y = 1144
integer width = 677
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
boolean enabled = false
string text = "Títulos de Condición"
end type

type cbx_id from checkbox within w_info_fumigaenc
integer x = 1463
integer y = 776
integer width = 229
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
string text = "ID"
end type

type cbx_lote from checkbox within w_info_fumigaenc
integer x = 1733
integer y = 776
integer width = 357
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
string text = "N° Lote"
end type

type st_10 from statictext within w_info_fumigaenc
integer x = 251
integer y = 1576
integer width = 1902
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_nuevo from checkbox within w_info_fumigaenc
integer x = 905
integer y = 1584
integer width = 567
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
string text = "Formato Nuevo"
end type

event clicked;
IF This.Checked THEN
	rb_2.Enabled 			= False
	rb_1.Enabled 			= False
	rb_3.Enabled 			= False
	rb_4.Enabled 			= False
	rb_5.Enabled 			= False
	cbx_titulo.Enabled 	= False
	cbx_sdp.Enabled 		= False
	cbx_mexicopredio.Enabled = False
	cbx_id.Enabled 		= False
	cbx_lote.Enabled 	= False
	
	cbx_mexicopredio.Checked = True
	cbx_id.Checked 	= True
	cbx_lote.Checked 	= True
	
ELSE
	rb_2.Enabled 			= True
	rb_1.Enabled 			= True
	rb_3.Enabled 			= True
	rb_4.Enabled 			= True
	rb_5.Enabled 			= True
	cbx_titulo.Enabled 	= True
	cbx_sdp.Enabled 		= True
	cbx_mexicopredio.Enabled = True
	cbx_id.Enabled 		= True
	cbx_lote.Enabled 	= True
	
	cbx_mexicopredio.Checked = False
	cbx_id.Checked 	= False
	cbx_lote.Checked 	= False
END IF	
end event

type dw_10 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2245
integer y = 284
integer width = 165
integer height = 148
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dw_detallefumisolicitud_control"
borderstyle borderstyle = stylelowered!
end type

type cbx_conex from checkbox within w_info_fumigaenc
integer x = 480
integer y = 1776
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

event clicked;IF This.Checked THEN
	rb_detalle.Enabled = True
	rb_detalle.Checked = True
	rb_solicitud.Enabled = True
	rb_solicitud.Checked = False
	rb_planilla.Enabled = True
	rb_planilla.Checked = False
ELSE
	rb_detalle.Enabled = False
	rb_detalle.Checked = False
	rb_solicitud.Enabled = False
	rb_solicitud.Checked = False
	rb_planilla.Enabled = False
	rb_planilla.Checked = False
END IF
end event

type rb_detalle from radiobutton within w_info_fumigaenc
integer x = 585
integer y = 1856
integer width = 1047
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
boolean enabled = false
string text = "Detalle del lote por Pallet"
end type

type rb_solicitud from radiobutton within w_info_fumigaenc
integer x = 585
integer y = 1936
integer width = 1248
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
boolean enabled = false
string text = "Solicitud Inspeccion Fitosanitaria S.A.G."
end type

type gb_3 from groupbox within w_info_fumigaenc
integer x = 325
integer y = 1732
integer width = 1765
integer height = 384
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_9 from statictext within w_info_fumigaenc
integer x = 251
integer y = 1672
integer width = 1902
integer height = 484
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_planilla from radiobutton within w_info_fumigaenc
integer x = 590
integer y = 2020
integer width = 1248
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
boolean enabled = false
string text = "Planilla Descripción Lote"
end type

type dw_11 from datawindow within w_info_fumigaenc
boolean visible = false
integer x = 2437
integer y = 292
integer width = 165
integer height = 148
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_prodconectivad_inscondi"
end type

