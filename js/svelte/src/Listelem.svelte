<script>
  import _ from 'underscore';

  import Listelem from './Listelem.svelte';
  
  export let data;
    
</script>

<ol> 
  {#each data as p}
    {#if _.isArray(p) }
      <Listelem data={p} />
    {:else}
      <li >{p.name}</li>
    {/if}
  {/each}
</ol>
